exception Error of string

let err s = raise (Error s)


let number value = match value with
  | Ast.Int n -> n
  |_ -> err("CastError: Not Integer")

let bool value = match value with
    | Ast.Bool n -> n
    | _ -> err("CastError: Not Boolean")

let string value = match value with
    | Ast.String n -> n
    | _ -> err("CastError: Not String")
let char value = match value with
    | Ast.Char n -> n
    | _ -> err("CastError: Not Char")

let float value = match value with
    | Ast.Float n -> n
    | _ -> err("CastError: Not Float")

let to_string expr = match expr with
    | Ast.Bool b -> b |> string_of_bool
    | Ast.Int i -> i |> string_of_int
    | _ -> err("Not to Display")

let rec eval expr = match expr with
  | Ast.String _ -> expr
  | Ast.Int  _ -> expr
  | Ast.Bool  _-> expr
  | Ast.Char  _-> expr
  | Ast.Float  _-> expr
  | Ast.Uminus n -> Ast.Int (-(number n))
  | Ast.Add (n,m) -> Ast.Int (number (eval n) +  number(eval m))
  | Ast.Sub (n,m) -> Ast.Int (number (eval n) - number(eval m))
  | Ast.Mul (n,m) -> Ast.Int (number (eval n) * number(eval m))
  | Ast.Div (n,m) -> let mm = number (eval m) in (
    match mm with 
    | 0 -> err("Division by zero")
    | _ -> Ast.Int ((number (eval n)) / mm)) 
 | Ast.Mod (n,m) -> let mm = number (eval m) in (
    match mm with 
    | 0 -> err("Division by zero")
    | _ -> Ast.Int ((number (eval n)) mod mm)) 
 