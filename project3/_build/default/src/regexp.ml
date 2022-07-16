open List
open Nfa

(*********)
(* Types *)
(*********)

type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp_t * regexp_t
  | Concat of regexp_t * regexp_t
  | Star of regexp_t

(***********)
(* Utility *)
(***********)

let fresh =
  let cntr = ref 0 in
  fun () ->
    cntr := !cntr + 1 ;
    !cntr

(*******************************)
(* Part 3: Regular Expressions *)
(*******************************)


let sigma_get nfa= 
  match nfa with {sigma; qs; q0; fs; delta} -> sigma;;

let qs_get nfa = 
  match nfa with {sigma; qs; q0; fs; delta} -> qs;;

let q0_get nfa = 
  match nfa with {sigma; qs; q0; fs; delta} -> q0;;

let fs_get nfa = 
  match nfa with {sigma; qs; q0; fs; delta} -> fs;;

let delta_get nfa = 
  match nfa with {sigma; qs; q0; fs; delta} -> delta;;

let create nsigma nqs nq0 nfs ndelta =
    {sigma = nsigma;
    qs = nqs;
    q0 = nq0;
    fs = nfs;
    delta = ndelta};;

let rec elem x a =
  match a with
  | h::t -> (h = x) || (elem x t)
  | [] -> false;;

let rec insert x a =
  if not (elem x a) then x::a else a;;

let insert_all xs a =
  List.fold_right insert xs a;;

let rec union a b =
  match a with
  | h::t -> insert h (union t b)
  | [] ->
    (match b with
     | h::t -> insert h (union [] t)
     | [] -> []);;

let rec regexp_to_nfa (regexp: regexp_t) : (int, char) nfa_t =
  match regexp with 
  | Empty_String -> let a = fresh() in create [] [a] a [a] [(a,None,a)]
  | Char c -> let a = fresh() in let b = fresh() in create [c] [a;b] a [b] [(a,Some c,b)]
  | Union (r1,r2) -> let a = fresh() in let b = fresh() in
    let rec1 = regexp_to_nfa r1 in 
    let rec2 = regexp_to_nfa r2 in 
    let new_sigma = union (sigma_get rec1) (sigma_get rec2) in
    let new_qs = union [a;b] (insert_all (qs_get rec1) (qs_get rec2)) in
    let new_delta = union (union (delta_get rec1) (delta_get rec2)) [(a, None, (q0_get rec1));(a, None, (q0_get rec2));(List.hd (fs_get rec1), None, b);(List.hd (fs_get rec2), None, b)] in
    create new_sigma new_qs a [b] new_delta
  | Concat (r1,r2) -> 
    let rec1 = regexp_to_nfa r1 in 
    let rec2 = regexp_to_nfa r2 in
    let new_sigma = union (sigma_get rec1) (sigma_get rec2) in
    let new_qs = union (qs_get rec1) (qs_get rec2) in
    let new_delta = union (union (delta_get rec1) (delta_get rec2)) [(List.hd (fs_get rec1), None, (q0_get rec2))] in
    create new_sigma new_qs (q0_get rec1) (fs_get rec2) new_delta
  | Star r -> let a = fresh() in let b = fresh() in
    let rec1 = regexp_to_nfa r in
    let new_delta = union (delta_get rec1) [(a,None,b); (b,None,a); (a,None, q0_get rec1); (List.hd (fs_get rec1),None,b)] in
    create (sigma_get rec1) (union (qs_get rec1) [a;b]) a [b] new_delta
;;

(*****************************************************************)
(* Below this point is parser code that YOU DO NOT NEED TO TOUCH *)
(*****************************************************************)

exception IllegalExpression of string

(* Scanner *)
type token =
  | Tok_Char of char
  | Tok_Epsilon
  | Tok_Union
  | Tok_Star
  | Tok_LParen
  | Tok_RParen
  | Tok_END

let tokenize str =
  let re_var = Str.regexp "[a-z]" in
  let re_epsilon = Str.regexp "E" in
  let re_union = Str.regexp "|" in
  let re_star = Str.regexp "*" in
  let re_lparen = Str.regexp "(" in
  let re_rparen = Str.regexp ")" in
  let rec tok pos s =
    if pos >= String.length s then [Tok_END]
    else if Str.string_match re_var s pos then
      let token = Str.matched_string s in
      Tok_Char token.[0] :: tok (pos + 1) s
    else if Str.string_match re_epsilon s pos then
      Tok_Epsilon :: tok (pos + 1) s
    else if Str.string_match re_union s pos then Tok_Union :: tok (pos + 1) s
    else if Str.string_match re_star s pos then Tok_Star :: tok (pos + 1) s
    else if Str.string_match re_lparen s pos then Tok_LParen :: tok (pos + 1) s
    else if Str.string_match re_rparen s pos then Tok_RParen :: tok (pos + 1) s
    else raise (IllegalExpression ("tokenize: " ^ s))
  in
  tok 0 str

let tok_to_str t =
  match t with
  | Tok_Char v -> Char.escaped v
  | Tok_Epsilon -> "E"
  | Tok_Union -> "|"
  | Tok_Star -> "*"
  | Tok_LParen -> "("
  | Tok_RParen -> ")"
  | Tok_END -> "END"

(*
   S -> A Tok_Union S | A
   A -> B A | B
   B -> C Tok_Star | C
   C -> Tok_Char | Tok_Epsilon | Tok_LParen S Tok_RParen

   FIRST(S) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(A) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(B) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(C) = Tok_Char | Tok_Epsilon | Tok_LParen
 *)

let parse_regexp (l : token list) =
  let lookahead tok_list =
    match tok_list with
    | [] -> raise (IllegalExpression "lookahead")
    | h :: t -> (h, t)
  in
  let rec parse_S l =
    let a1, l1 = parse_A l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Union ->
        let a2, l2 = parse_S n in
        (Union (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_A l =
    let a1, l1 = parse_B l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Char c ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_Epsilon ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_LParen ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_B l =
    let a1, l1 = parse_C l in
    let t, n = lookahead l1 in
    match t with Tok_Star -> (Star a1, n) | _ -> (a1, l1)
  and parse_C l =
    let t, n = lookahead l in
    match t with
    | Tok_Char c -> (Char c, n)
    | Tok_Epsilon -> (Empty_String, n)
    | Tok_LParen ->
        let a1, l1 = parse_S n in
        let t2, n2 = lookahead l1 in
        if t2 = Tok_RParen then (a1, n2)
        else raise (IllegalExpression "parse_C 1")
    | _ -> raise (IllegalExpression "parse_C 2")
  in
  let rxp, toks = parse_S l in
  match toks with
  | [Tok_END] -> rxp
  | _ -> raise (IllegalExpression "parse didn't consume all tokens")


let string_to_regexp str = parse_regexp @@ tokenize str

let string_to_nfa str = regexp_to_nfa @@ string_to_regexp str
