open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

(*
and expr =
  | Value of value
  | ID of var
  | Fun of var * expr 
  | Not of expr
  | Binop of op * expr * expr
  | If of expr * expr * expr
  | FunctionCall of expr * expr
  | Let of var * bool * expr * expr 
 *)

(*Expr -> LetExpr | IfExpr | FunctionExpr | OrExpr *)
let rec parse_expr toks =
  match (lookahead toks) with 
  | Some Tok_Let -> parse_LetExpr toks
  | Some Tok_If -> parse_IfExpr toks
  | Some Tok_Fun -> parse_FunctionExpr toks
  | _ -> parse_OrExpr toks

(* LetExpr -> let Recursion Tok_ID = Expr in Exp *)
and parse_LetExpr toks =
  let (toks_parse_rec, exprs_rec) = parse_Recursion (match_token toks Tok_Let) in
  let tok_id = get_ID toks_parse_rec in
  let toks_equal = match_token (match_token toks_parse_rec (Tok_ID tok_id)) Tok_Equal in
  let (toks_parse_equ, exprs_equ) = parse_expr toks_equal in
  let (toks_parse_in, exprs_in) = parse_expr (match_token toks_parse_equ Tok_In) in
  (toks_parse_in, Let (tok_id, exprs_rec, exprs_equ, exprs_in))

and get_ID toks =
  match lookahead toks with
  | Some Tok_ID a -> a
  | _ -> raise (InvalidInputException "id")

(*Recursion -> rec | ε*)
and parse_Recursion toks = 
  match lookahead toks with
  Some Tok_Rec -> ((match_token toks Tok_Rec), true)
  | _ -> (toks, false)

(*FunctionExpr -> fun Tok_ID -> Expr*)
and parse_FunctionExpr toks =
  let tok_id = get_ID (match_token toks Tok_Fun) in
  let toks_id = match_token (match_token toks Tok_Fun) (Tok_ID tok_id) in
  let (toks_parse_arrow, exprs_arrow) = parse_expr (match_token toks_id Tok_Arrow) in 
  (toks_parse_arrow, Fun(tok_id, exprs_arrow))

(*IfExpr -> if Expr then Expr else Expr*)
and parse_IfExpr toks =
  let (toks_parse_if, exprs_if) = parse_expr (match_token toks Tok_If) in
  let (toks_parse_then, exprs_then) = parse_expr (match_token toks_parse_if Tok_Then) in
  let (toks_parse_else, exprs_else) = parse_expr (match_token toks_parse_then Tok_Else) in
  (toks_parse_else, If(exprs_if, exprs_then, exprs_else))

(*OrExpr -> AndExpr || OrExpr | AndExpr*)
and parse_OrExpr toks =
  let (toks_parse_and, exprs_and) = parse_AndExpr toks in
  match lookahead toks_parse_and with
  | Some Tok_Or -> 
    let (toks_parse_or, exprs_or) = parse_OrExpr (match_token toks_parse_and Tok_Or) in 
    (toks_parse_or, Binop(Or, exprs_and, exprs_or))
  | _ -> (toks_parse_and, exprs_and)

(*AndExpr -> EqualityExpr && AndExpr | EqualityExpr*)
and parse_AndExpr toks = 
  let (toks_parse_equality, exprs_equality) = parse_EqualityExpr toks in
  match lookahead toks_parse_equality with
  | Some Tok_And -> 
    let (toks_parse_and, exprs_add) = parse_AndExpr (match_token toks_parse_equality Tok_And) in 
    (toks_parse_and, Binop(And, exprs_equality, exprs_add))
  | _ -> (toks_parse_equality, exprs_equality)

(*EqualityExpr -> RelationalExpr EqualityOperator EqualityExpr | RelationalExpr*)
and parse_EqualityExpr toks = 
  let (toks_parse_rela, exprs_rela) = parse_RelationalExpr toks in
  parse_EqualityOperator toks_parse_rela exprs_rela

(*EqualityOperator -> = | <>*)
and parse_EqualityOperator toks_rela exprs_rela =
  match lookahead toks_rela with 
  | Some Tok_Equal -> 
    let (toks_parse_equal, exprs_equal) = parse_EqualityExpr (match_token toks_rela Tok_Equal) in 
    (toks_parse_equal, Binop(Equal, exprs_rela, exprs_equal))
  | Some Tok_NotEqual -> 
    let(toks_parse_notequal, exprs_notequal) = parse_EqualityExpr (match_token toks_rela Tok_NotEqual) in 
    (toks_parse_notequal, Binop(NotEqual, exprs_rela, exprs_notequal))
  | _ -> (toks_rela, exprs_rela)

(*RelationalExpr -> AdditiveExpr RelationalOperator RelationalExpr | AdditiveExpr*)
and parse_RelationalExpr toks =
  let (toks_parse_add, exprs_add) = parse_AdditiveExpr toks in
  parse_RelationalOperators toks_parse_add exprs_add

(*RelationalOperator -> < | > | <= | >=*)
and parse_RelationalOperators toks_add exprs_add = 
  match lookahead toks_add with
  | Some Tok_Less -> 
    let (toks_parse_less, exprs_less) = parse_RelationalExpr (match_token toks_add Tok_Less) in
    (toks_parse_less, Binop(Less, exprs_add, exprs_less))
  | Some Tok_Greater -> 
    let (toks_parse_great, exprs_great) = parse_RelationalExpr (match_token toks_add Tok_Greater) in
    (toks_parse_great, Binop(Greater, exprs_add, exprs_great))
  | Some Tok_LessEqual -> 
    let (toks_parse_lessEq, exprs_lessEq) = parse_RelationalExpr (match_token toks_add Tok_LessEqual) in 
    (toks_parse_lessEq, Binop(LessEqual, exprs_add, exprs_lessEq))
  | Some Tok_GreaterEqual -> 
    let (toks_parse_greEq, exprs_greEq) = parse_RelationalExpr (match_token toks_add Tok_GreaterEqual) in
    (toks_parse_greEq, Binop(Less, exprs_add, exprs_greEq))
  | _ -> (toks_add, exprs_add)

(*AdditiveExpr -> MultiplicativeExpr AdditiveOperator AdditiveExpr | MultiplicativeExpr*)
and parse_AdditiveExpr toks = 
  let (toks_parse_mult, exprs_mult) = parse_MultiplicativeExpr toks in
  parse_AdditiveOperators toks_parse_mult exprs_mult

(*AdditiveOperator -> + | -*)
and parse_AdditiveOperators toks_mult exprs_mult =
  match lookahead toks_mult with
  | Some Tok_Add -> 
    let (toks_parse_add, exprs_add) = parse_AdditiveExpr (match_token toks_mult Tok_Add) in 
    (toks_parse_add, Binop(Add, exprs_mult, exprs_add))
  | Some Tok_Sub -> 
    let (toks_parse_sub, exprs_sub) = parse_AdditiveExpr (match_token toks_mult Tok_Sub) in 
    (toks_parse_sub, Binop(Sub, exprs_mult, exprs_sub))
  | _ -> (toks_mult, exprs_mult)

(*MultiplicativeExpr -> ConcatExpr MultiplicativeOperator MultiplicativeExpr | ConcatExpr*)
and parse_MultiplicativeExpr toks = 
  let (toks_parse_concat, exprs_concat) = parse_ConcatExpr toks in
  parse_MultiplicativeOperator toks_parse_concat exprs_concat 

(*MultiplicativeOperator -> * | /*)
and parse_MultiplicativeOperator toks_concat exprs_concat =
  match lookahead toks_concat with 
  | Some Tok_Mult -> 
    let (toks_parse_mult, exprs_mult) = parse_MultiplicativeExpr (match_token toks_concat Tok_Mult) in 
    (toks_parse_mult, Binop (Mult, exprs_concat, exprs_mult))
  | Some Tok_Div -> 
    let (toks_parse_div, exprs_div) = parse_MultiplicativeExpr (match_token toks_concat Tok_Div) in 
    (toks_parse_div, Binop (Div, exprs_concat, exprs_div))  
  |_ -> (toks_concat, exprs_concat)

(*ConcatExpr -> UnaryExpr ^ ConcatExpr | UnaryExpr*)
and parse_ConcatExpr toks =
  let (toks_parse_unary, exprs_unary) = parse_UnaryExpr toks in
  match lookahead toks_parse_unary with 
  | Some Tok_Concat -> 
    let (toks_parse_concat, exprs_concat) = parse_ConcatExpr (match_token toks_parse_unary Tok_Concat) in 
    (toks_parse_concat, Binop(Concat, exprs_unary, exprs_concat))
  | _ -> (toks_parse_unary, exprs_unary)

(*UnaryExpr -> not UnaryExpr | FunctionCallExpr*)
and parse_UnaryExpr toks = 
  match lookahead toks with 
  | Some Tok_Not ->
    let (toks_parse_unary, exprs_unary) = parse_UnaryExpr (match_token toks Tok_Not) in 
    (toks_parse_unary, Not (exprs_unary))
  | _ -> parse_FunctionExprCallExpr toks

(*FunctionCallExpr -> PrimaryExpr PrimaryExpr | PrimaryExpr*)
and parse_FunctionExprCallExpr toks = 
  let (toks_parse_prim, exprs_prim) = parse_PrimaryExpr toks in 
  match lookahead toks_parse_prim with 
  | Some Tok_Int a -> 
    let (toks_parse_primp, exprs_primp) = parse_PrimaryExpr toks_parse_prim in 
    (toks_parse_primp, FunctionCall(exprs_prim, exprs_primp))
  | Some Tok_Bool a -> 
    let (toks_parse_primp, exprs_primp) = parse_PrimaryExpr toks_parse_prim in 
    (toks_parse_primp, FunctionCall(exprs_prim, exprs_primp))
  | Some Tok_String a -> 
    let (toks_parse_primp, exprs_primp) = parse_PrimaryExpr toks_parse_prim in 
    (toks_parse_primp, FunctionCall(exprs_prim, exprs_primp))
  | Some Tok_ID a -> 
    let (toks_parse_primp, exprs_primp) = parse_PrimaryExpr toks_parse_prim in 
    (toks_parse_primp, FunctionCall(exprs_prim, exprs_primp))
  | Some Tok_LParen -> 
    let (toks_parse_primp, exprs_primp) = parse_PrimaryExpr toks_parse_prim in 
    (toks_parse_primp, FunctionCall(exprs_prim, exprs_primp))
  | _ -> (toks_parse_prim, exprs_prim)

(*PrimaryExpr -> Tok_Int | Tok_Bool | Tok_String | Tok_ID | ( Expr )*)
and parse_PrimaryExpr toks = 
    match lookahead toks with 
    | Some Tok_Int a -> 
    ((match_token toks (Tok_Int a)), Value(Int a))
    | Some Tok_Bool a -> 
    ((match_token toks (Tok_Bool a)), Value(Bool a))
    | Some Tok_String a -> 
    ((match_token toks (Tok_String a)), Value(String a))
    | Some Tok_ID a -> 
    ((match_token toks (Tok_ID a)), ID a)
    | Some Tok_LParen -> 
      let (toks_parse, exprs) = parse_expr (match_token toks Tok_LParen) in
      ((match_token toks_parse Tok_RParen), exprs)
    | _ -> raise (InvalidInputException"primary")

(* Part 3: Parsing mutop *)

(*Mutop -> DefMutop | ExprMutop | ;;*)
let rec parse_mutop toks = 
  match lookahead toks with 
  | Some Tok_Def -> parse_DefMutop toks
  | Some Tok_DoubleSemi -> ((match_token toks Tok_DoubleSemi), NoOp)
  | _ -> parse_ExprMutop toks

(*DefMutop -> def Tok_ID = Expr ;;*)
and parse_DefMutop toks = 
    let (toks_def, id) = 
    match (lookahead (match_token toks Tok_Def)) with
      | Some Tok_ID a -> (match_token (match_token toks Tok_Def) (Tok_ID a), a)
      | _ ->  raise (InvalidInputException ("def_mutop"))
    in 
    let (toks_parse, exprs) = parse_expr (match_token toks_def Tok_Equal) in
    ((match_token toks_parse Tok_DoubleSemi), Def(id , exprs)) 

(*ExprMutop -> Expr ;;*)
and parse_ExprMutop toks =
  let (a, exprs) = parse_expr toks in 
  ((match_token a Tok_DoubleSemi), Expr(exprs))
  