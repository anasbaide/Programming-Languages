open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input =
    (*MicroCaml syntax with its corresponding regex is shown below*)
    let re_Int = Str.regexp "[0-9]+" in
    let re_NegInt = Str.regexp "(-[0-9]+)" in
    let re_LParen = Str.regexp "(" in
    let re_RParen = Str.regexp ")" in
    let re_Equal = Str.regexp "=" in
    let re_NotEqual = Str.regexp "<>" in
    let re_Greater = Str.regexp ">" in
    let re_Less = Str.regexp "<" in
    let re_GreaterEqual = Str.regexp ">=" in
    let re_LessEqual = Str.regexp "<=" in
    let re_Or = Str.regexp "||" in
    let re_And = Str.regexp "&&" in
    let re_Not = Str.regexp "not" in
    let re_If = Str.regexp "if" in
    let re_Then = Str.regexp "then" in
    let re_Else = Str.regexp "else" in
    let re_Add = Str.regexp "\\+" in
    let re_Sub = Str.regexp "\\-" in
    let re_Mult = Str.regexp "\\*" in
    let re_Div = Str.regexp "/" in
    let re_Concat = Str.regexp "\\^" in
    let re_Let = Str.regexp "let" in
    let re_Def = Str.regexp "def" in
    let re_In = Str.regexp "in" in
    let re_Rec = Str.regexp "rec" in
    let re_Fun = Str.regexp "fun" in
    let re_Arrow = Str.regexp "->" in
    let re_DoubleSemi = Str.regexp ";;" in
    let re_WhiteSpaces = Str.regexp "[ \n\t]+" in
    let re_Bool = Str.regexp "false\\|true" in
    let re_String = Str.regexp "\"[^\"]*\"" in
    let re_ID = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" in

    let rec tok pos s =
        if pos >= String.length s then 
            []
        else
            if (Str.string_match re_NegInt s pos) then
                let token = Str.matched_string s in
                let token2 = String.sub token 1 ((String.length token) - 1) in
                (Tok_Int (int_of_string token2))::(tok (pos+(String.length token)) s)
            else if (Str.string_match re_Int s pos) then
                let token = Str.matched_string s in
                (Tok_Int (int_of_string token))::(tok (pos+(String.length token)) s)
            else if (Str.string_match re_LParen s pos) then
                (Tok_LParen)::(tok (pos+1) s)
            else if (Str.string_match re_RParen s pos) then
                (Tok_RParen)::(tok (pos+1) s)
            else if (Str.string_match re_Arrow s pos) then
                (Tok_Arrow)::(tok (pos+2) s)
            else if (Str.string_match re_Equal s pos) then
                (Tok_Equal)::(tok (pos+1) s)
            else if (Str.string_match re_NotEqual s pos) then
                (Tok_NotEqual)::(tok (pos+2) s)
            else if (Str.string_match re_Greater s pos) then
                (Tok_Greater)::(tok (pos+1) s) 
            else if (Str.string_match re_Less s pos) then
                (Tok_Less)::(tok (pos+1) s)  
            else if (Str.string_match re_GreaterEqual s pos) then
                (Tok_GreaterEqual)::(tok (pos+2) s)
            else if (Str.string_match re_LessEqual s pos) then
                (Tok_LessEqual)::(tok (pos+2) s)
            else if (Str.string_match re_Or s pos) then
                (Tok_Or)::(tok (pos+2) s)
            else if (Str.string_match re_And s pos) then
                (Tok_And)::(tok (pos+2) s)
            else if (Str.string_match re_Not s pos) then
                (Tok_Not)::(tok (pos+3) s) 
            else if (Str.string_match re_If s pos) then
                (Tok_If)::(tok (pos+2) s) 
            else if (Str.string_match re_Then s pos) then
                (Tok_Then)::(tok (pos+4) s) 
            else if (Str.string_match re_Else s pos) then
                (Tok_Else)::(tok (pos+4) s)
            else if (Str.string_match re_Add s pos) then
                (Tok_Add)::(tok (pos+1) s)  
            else if (Str.string_match re_Sub s pos) then
                (Tok_Sub)::(tok (pos+1) s) 
            else if (Str.string_match re_Mult s pos) then
                (Tok_Mult)::(tok (pos+1) s) 
            else if (Str.string_match re_Div s pos) then
                (Tok_Div)::(tok (pos+1) s) 
            else if (Str.string_match re_Concat s pos) then
                (Tok_Concat)::(tok (pos+1) s)
            else if (Str.string_match re_Let s pos) then
                (Tok_Let)::(tok (pos+3) s)
            else if (Str.string_match re_Def s pos) then
                (Tok_Def)::(tok (pos+3) s)
            else if (Str.string_match re_In s pos) then
                (Tok_In)::(tok (pos+2) s)
            else if (Str.string_match re_Rec s pos) then
                (Tok_Rec)::(tok (pos+3) s)
            else if (Str.string_match re_Fun s pos) then
                (Tok_Fun)::(tok (pos+3) s)
            else if (Str.string_match re_DoubleSemi s pos) then
                (Tok_DoubleSemi)::(tok (pos+2) s)
            else if (Str.string_match re_Bool s pos) then
                let token = Str.matched_string s in
                (Tok_Bool (bool_of_string token))::(tok (pos+(String.length token)) s)
            else if (Str.string_match re_String s pos) then
                let token = Str.matched_string s in
                let token2 = String.sub token 1 ((String.length token)-2) in
                (Tok_String token2)::(tok (pos+(String.length token)) s)
            else if (Str.string_match re_ID s pos) then
                let token = Str.matched_string s in
                (Tok_ID token)::(tok (pos+(String.length token)) s)
            else if (Str.string_match re_WhiteSpaces s pos) then
                let token = Str.matched_string s in
                (tok (pos+(String.length token)) s)
            else
                raise (InvalidInputException "tokenize")
    in
    tok 0 input
