open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let compare a b = if a < b then -1 else if a = b then 0 else 1;;

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

let rec elem_lst lst1 lst2 =  
  match lst1 with 
  | [] -> false
  | h::t -> if (elem h lst2) then true else elem_lst t lst2 ;;

let transitions nfa qs s =
  List.fold_left (fun acc (start, trans, final) -> if elem start qs && trans = s then (insert final acc) else acc) [] (delta_get nfa);;

let transition_one nfa elem s =
  List.fold_left (fun acc (start, trans, final) -> if elem = start &&– trans = s then (insert final acc) else acc) [] (delta_get nfa);;

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  let lst = (transitions nfa qs s) in List.sort compare lst;;

let rec closure_help nfa qs ret=
  match qs with
  |[] -> []
  |h::t -> if not(elem h ret) then
    let trans = transition_one nfa h None in 
    insert h (closure_help nfa (insert_all t trans) (insert h ret))
    else (closure_help nfa t ret);;

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  List.sort compare (closure_help nfa qs []);;

let rec accept_help nfa q0s lst = 
	match lst with
	| [] -> if (elem_lst (e_closure nfa q0s) (fs_get nfa)) then true else false
	| h::t -> accept_help nfa (move nfa (e_closure nfa q0s) (Some h)) t;;

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
	accept_help nfa ([q0_get nfa]) (explode s);;

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  List.fold_left (fun acc h -> (insert_all ([e_closure nfa (move nfa qs (Some h))]) acc)) [] (sigma_get nfa);;

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  List.fold_left (fun acc h -> (insert_all ([(qs, Some h, e_closure nfa (move nfa qs (Some h)))]) acc)) [] (sigma_get nfa);;

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  List.fold_left (fun acc h -> if (elem h (fs_get nfa)) then (insert_all [qs] acc) else acc) [] qs;;

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  match work with
  |[] -> let new_fs = List.fold_left (fun acc h -> union acc (new_finals nfa h)) [] (qs_get dfa) in
        let new_delta = List.fold_left (fun acc h -> insert h acc) [] (delta_get dfa) in
        (create (sigma_get dfa) (qs_get dfa) (q0_get dfa) new_fs new_delta)
  |h::t -> let new_qs = List.fold_left (fun acc h -> insert h acc) (qs_get dfa) (new_states nfa h) in
          let new_delta = union (new_trans nfa h) (delta_get dfa) in
          let new_work = union t (List.fold_left (fun acc h -> if not(elem h (qs_get dfa)) then h::acc else acc) [] (new_states nfa h)) in
          let new_dfa = (create (sigma_get dfa) new_qs (q0_get dfa) (fs_get dfa) new_delta) in
          nfa_to_dfa_step nfa new_dfa new_work;;

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  nfa_to_dfa_step nfa (create (sigma_get nfa) [e_closure nfa [q0_get nfa]] (e_closure nfa [q0_get nfa]) [] []) ([e_closure nfa [q0_get nfa]]);;
