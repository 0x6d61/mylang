open Util

let rec eval expr = match expr with
  | Ast.String _ -> expr
  | Ast.Int  _ -> expr
  | Ast.Bool  _-> expr
  | Ast.Char  _-> expr
  | Ast.Float  _-> expr
  | Ast.Uminus n -> (match n with
      | Ast.Int _ -> Ast.Int (-(number n))
      | Ast.Float _ -> Ast.Float (-.(float n))
      | _ -> err("Not Called"))
  | Ast.Add (n,m) -> add (eval n) (eval m) 
  | Ast.Sub (n,m) -> sub (eval n) (eval m)
  | Ast.Mul (n,m) -> mul (eval n ) (eval m)
  | Ast.Div (n,m) -> div (eval n) (eval m)
  | Ast.Mod (n,m) -> modd (eval n) (eval m)