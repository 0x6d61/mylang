open Syntax
open Env
open Util

let rec eval env expr = match expr with
  | Ast.String _ -> expr
  | Ast.Int  _ -> expr
  | Ast.Bool  _-> expr
  | Ast.Char  _-> expr
  | Ast.Float  _-> expr
  | Ast.Uminus n -> (match n with
      | Ast.Int _ -> Ast.Int (-(number n))
      | Ast.Float _ -> Ast.Float (-.(float n))
      | _ -> err("Not Called"))
  | Ast.Add (n,m) -> add (eval env n) (eval env m) 
  | Ast.Sub (n,m) -> sub (eval env n) (eval env m)
  | Ast.Mul (n,m) -> mul (eval env n ) (eval env m)
  | Ast.Div (n,m) -> div (eval env n) (eval env m)
  | Ast.Mod (n,m) -> modd (eval env n) (eval env m)
  | Ast.Eq (n,m) -> eq (eval env n) (eval env m)
  | Ast.NotEq (n,m) -> noteq (eval env n) (eval env m) 
  | Ast.Lt (n,m) -> lt (eval env n) (eval env m)
  | Ast.Le (n,m) -> le (eval env n) (eval env m)
  | Ast.Gt (n,m) -> gt (eval env n) (eval env m)
  | Ast.Ge (n,m) -> ge (eval env n) (eval env m)
  | Ast.If (expr,true_expr,false_expr) -> eval env ( iff (eval env expr) true_expr false_expr)
  | Ast.Ident n  -> get_env n env
  | Ast.CallFunc (func_name,args) -> try 
        let (v_args,body) = get_func (func_name |> to_string) env
    in let env1 = set_vargs  (List.map (fun x -> to_string x) v_args) (List.map (eval env) args) env in
    eval env1 body
  with Error _ -> let func = get_build_in_func (func_name |> to_string) build_in_func
                    in func (List.map (eval env) args)