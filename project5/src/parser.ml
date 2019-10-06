open SmallCTypes
open Utils
open TokenTypes

type stmt_result = token list * stmt
type expr_result = token list * expr
exception ParseError of string
let tok_list = ref []

(* Provided helper function - takes a token list and an expected token.
 * Handles error cases and returns the tail of the list *)
let match_token a =
  match !tok_list with
  (* checks lookahead; advances on match *)
  | (h::t) when a = h -> tok_list := t
  | _ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s"
        (string_of_token a)
        (string_of_list string_of_token !tok_list)
    ))
;;

let lookahead () =
  match !tok_list with
    [] -> raise (ParseError "no tokens")
  | (h::t) -> h
;;

(* start your code here *)

let rec or_exp () =
let a1 = a_exp() in
let t = lookahead() in
match t with
| Tok_Or -> match_token Tok_Or;
let a2 = or_exp() in Or(a1,a2)
| _ -> a1

and a_exp () =
let a1 = eql_expr() in
let t = lookahead() in
match t with
| Tok_And -> match_token Tok_And;
let a2 = a_exp() in And(a1,a2)
| _ -> a1

and eql_expr () =
let a1 = rl_expr() in
let t = lookahead() in
match t with
| Tok_Equal -> match_token Tok_Equal;
let a2 = eql_expr() in Equal(a1,a2)
| Tok_NotEqual -> match_token Tok_NotEqual;
let a2 = eql_expr() in NotEqual(a1,a2)
| _ -> a1

and rl_expr () =
  let a1 = add_expr() in
  let t = lookahead() in
  match t with
  | Tok_Less -> match_token Tok_Less;
    let a2 = rl_expr() in Less(a1,a2)
  | Tok_Greater -> match_token Tok_Greater;
    let a2 = rl_expr() in Greater(a1,a2)
  | Tok_LessEqual -> match_token Tok_LessEqual;
    let a2 = rl_expr() in LessEqual(a1,a2)
  | Tok_GreaterEqual -> match_token Tok_GreaterEqual;
    let a2 = rl_expr() in GreaterEqual(a1,a2)
  | _ -> a1

and add_expr () =
let a1 = mult_expr() in
let t = lookahead() in
match t with
| Tok_Add -> match_token Tok_Add;
let a2 = add_expr() in Add(a1,a2)
| Tok_Sub -> match_token Tok_Sub;
let a2 = add_expr() in Sub(a1,a2)
| _ -> a1


and mult_expr () =
let a1 = pow_expr() in
let t = lookahead() in
match t with
| Tok_Mult -> match_token Tok_Mult;
let a2 = mult_expr() in Mult(a1,a2)
| Tok_Div -> match_token Tok_Div;
let a2 = mult_expr() in Div(a1,a2)
| _ -> a1


and pow_expr () =
let a1 = un_expr() in
let t = lookahead() in
match t with
| Tok_Pow -> match_token Tok_Pow;
let a2 = pow_expr() in Pow(a1,a2)
| _ -> a1

and un_expr () =
let t = lookahead() in
match t with
| Tok_Not -> match_token Tok_Not;
  Not(un_expr())
| _ -> prim_expr()

and prim_expr () =
let t = lookahead() in
match t with
| Tok_Int c -> let _ = match_token  (Tok_Int c) in
  Int (c)
| Tok_Bool c -> let _ = match_token  (Tok_Bool c) in
  Bool (c)
| Tok_ID c -> let _ = match_token (Tok_ID c) in
  ID (c)
| Tok_LParen ->
  let _ = match_token Tok_LParen in let a = or_exp() in
   let _ = match_token Tok_RParen in a
| _ -> raise (ParseError (string_of_token (let _ = match_token (lookahead()) in let _ = match_token (lookahead()) in let _ = match_token (lookahead()) in let _ = match_token (lookahead()) in let _ = match_token (lookahead()) in lookahead())))
;;

let rec parse_expr toks =
tok_list :=  toks;
let a1 = or_exp() in (!tok_list,a1)
;;

let rec start() =
  let a1 = stoption() in if (a1 = NoOp) then NoOp else
  let t = lookahead() in
  match t with
  | Tok_Int_Type -> Seq(a1,start())
  | Tok_Bool_Type -> Seq(a1,start())
  | Tok_ID a -> Seq(a1,start())
  | Tok_Print -> Seq(a1,start())
  | Tok_If -> Seq(a1,start())
  | Tok_Do -> Seq(a1,start())
  | Tok_While -> Seq(a1,start())
  | _ -> Seq(a1,NoOp)

and stoption() = let t = lookahead() in
  match t with
  | Tok_Int_Type -> let _ = match_token Tok_Int_Type in
    let x = Declare(Int_Type,declst()) in let _ = match_token Tok_Semi in x
  | Tok_Bool_Type -> let _ = match_token Tok_Bool_Type in
    let x = Declare(Bool_Type,declst()) in let _ = match_token Tok_Semi in x
  | Tok_ID a -> let _ = match_token (Tok_ID a) in let _ = match_token Tok_Assign
    in let y = or_exp() in let _ = match_token Tok_Semi in
    Assign(a, y)
  | Tok_Print -> let _ = match_token Tok_Print in let _ = match_token Tok_LParen in
    let w = or_exp() in let _ = match_token Tok_RParen in
    let _ = match_token Tok_Semi in Print(w)
  | Tok_If -> let _ = match_token Tok_If in let _ = match_token Tok_LParen in
    let z = or_exp() in let _ = match_token Tok_RParen in
    let _ = match_token Tok_LBrace in let w = start() in let _ = match_token Tok_RBrace in
    let r = elsest() in If(z,w,r)
  | Tok_Do -> let _ = match_token Tok_Do in let _ = match_token Tok_LBrace in
    let q = start() in let _ = match_token Tok_RBrace in let _ = match_token Tok_While in
    let _ = match_token Tok_LParen in let z = or_exp() in let _ = match_token Tok_RParen in
    let _ = match_token Tok_Semi in DoWhile(q,z)
  | Tok_While ->let _ = match_token Tok_While in let _ = match_token Tok_LParen in
    let z = or_exp() in let _ = match_token Tok_RParen in
    let _ = match_token Tok_LBrace in let q = start() in let _ = match_token Tok_RBrace
    in While(z,q)
  | _ -> NoOp

and declst() = let t = lookahead() in
    match t with
    | Tok_ID a -> let _ = match_token (Tok_ID a) in a
    | _ -> raise (ParseError "Nada2")

and elsest() = let t = lookahead() in
  match t with
  | Tok_Else -> let _ = match_token Tok_Else in let _ = match_token Tok_LBrace in
    let x = start() in let _ = match_token Tok_RBrace in x
  | _ -> NoOp
;;

let parse_stmt toks =
  tok_list :=  toks;
  let a1 = start() in (!tok_list,a1);;

let parse_main toks = tok_list :=  toks;
  let _ = match_token Tok_Int_Type in
  let _ = match_token Tok_Main in
  let _ = match_token Tok_LParen in
  let _ = match_token Tok_RParen in
  let _ = match_token Tok_LBrace in
  let cc = start() in
  let _ = match_token Tok_RBrace in
  let _ = match_token EOF in cc
;;
