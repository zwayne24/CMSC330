open SmallCTypes
open EvalUtils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let rec eval_expr env e = match e with
  | Int a -> Int_Val a
  | Bool a -> Bool_Val a
  | ID a -> if (List.mem_assoc a env) then List.assoc a env
    else raise(DeclareError "No Sir")
  | Add (a,b) -> (match eval_expr env a,eval_expr env b with
      | Int_Val x, Int_Val y ->  Int_Val(x + y)
    | _,_ -> raise(TypeError "No can do"))
  | Sub (a,b) -> (match eval_expr env a,eval_expr env b with
      | Int_Val x, Int_Val y ->  Int_Val(x - y)
      | _,_ -> raise(TypeError "No can do"))
  | Mult (a,b) -> (match eval_expr env a,eval_expr env b with
      | Int_Val x, Int_Val y -> Int_Val(x * y)
      | _,_ -> raise(TypeError "No can do"))
  | Div (a,b) -> (match eval_expr env a,eval_expr env b with
      | Int_Val x, Int_Val y ->  if y = 0 then
          raise(DivByZeroError) else Int_Val(x / y)
      | _,_ -> raise(TypeError "No can do"))
  | Pow (a,b) -> (match eval_expr env a,eval_expr env b with
      | Int_Val x, Int_Val y ->  Int_Val(let rec pow (o,t) = match t with
          | 0 -> 1
          | 1 -> o
          | n -> o * pow (o,t-1)
         in pow(x,y))
      | _,_ -> raise(TypeError "No can do"))
  | Or (a,b) -> (match eval_expr env a,eval_expr env b with
      | Bool_Val x, Bool_Val y ->  Bool_Val(x || y)
      | _,_ -> raise(TypeError "No can do"))
  | And (a,b) -> (match eval_expr env a,eval_expr env b with
      | Bool_Val x, Bool_Val y ->  Bool_Val(x && y)
      | _,_ -> raise(TypeError "No can do"))
  | Not a -> (match eval_expr env a with
      | Bool_Val x -> if x then Bool_Val(false) else Bool_Val(true)
      | _ -> raise(TypeError "No can do"))
  | Greater (a,b) -> (match eval_expr env a,eval_expr env b with
      | Int_Val x, Int_Val y ->  Bool_Val(x > y)
      | _,_ -> raise(TypeError "No can do"))
  | Less (a,b) -> (match eval_expr env a,eval_expr env b with
      | Int_Val x, Int_Val y ->  Bool_Val(x < y)
      | _,_ -> raise(TypeError "No can do"))
  | GreaterEqual (a,b) -> (match eval_expr env a,eval_expr env b with
      | Int_Val x, Int_Val y ->  Bool_Val(x >= y)
      | _,_ -> raise(TypeError "No can do"))
  | LessEqual (a,b) -> (match eval_expr env a,eval_expr env b with
      | Int_Val x, Int_Val y ->  Bool_Val(x <= y)
      | _,_ -> raise(TypeError "No can do"))
  | Equal (a,b) -> (match eval_expr env a,eval_expr env b with
      | Int_Val x, Int_Val y ->  Bool_Val(x = y)
      | _,_ -> raise(TypeError "No can do"))
  | NotEqual (a,b) -> (match eval_expr env a,eval_expr env b with
      | Int_Val x, Int_Val y ->  Bool_Val(x != y)
      | _,_ -> raise(TypeError "No can do"))
;;

let change name nw env = (name,nw)::(List.remove_assoc name env);;

let rec eval_stmt env s = match s with
  | NoOp -> env
  | Seq (a,b) -> let env' = eval_stmt env a in eval_stmt env' b
  | Declare (d,v) -> if (List.mem_assoc v env) then
      raise(DeclareError "Not Again") else (match d with
      | Int_Type ->  (v,Int_Val(0)) :: env
      | Bool_Type -> (v,Bool_Val(false)) :: env)
  | Assign (v,ex) -> if (List.mem_assoc v env) then
      (match List.assoc v env, eval_expr env ex with
       | Int_Val a,Int_Val b -> change v eval_expr env ex env
       | Bool_Val a, Bool_Val b -> change v eval_expr env ex env
       | _,_ -> raise(TypeError "Not so fast"))
    else raise(DeclareError "Bruh, Declare me first")
  | If (ex,i,e) -> (match eval_expr env ex with
      | Bool_val true -> eval_stmt env i
      | Bool_val false -> eval_stmt env e
      | _ -> raise(TypeError "You tried"))
  | While (ex,st) -> let rec whilefun exp sta en = match eval_expr en exp with
      | Bool_val true -> whilefun exp sta (eval_stmt en sta)
      | Bool_val false -> en
      | _ -> raise(TypeError "You tried")
    in whilefun ex st env
  | DoWhile (st,ex) -> let rec whilefun2 exp sta en = match eval_expr en exp with
      | Bool_val true -> whilefun2 exp sta (eval_stmt en sta)
      | Bool_val false -> en
      | _ -> raise(TypeError "You tried")
    in whilefun ex st (eval_stmt env st)
  | Print e -> match eval_expr e with
    | Bool_Val a -> print_output_bool(a);print_output_newline()
    | Int_Val a -> print_output_int(a);print_output_newline()
;;
