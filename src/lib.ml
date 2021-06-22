open Syntax
open Env
open Util
open Error

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
  | Ast.Env _ -> expr
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
  | Ast.If (expr,true_expr,false_expr) -> if iff (eval env expr) then
      true_expr |> List.map (eval env) |> List.rev |> List.hd
    else
      eval env false_expr
  | Ast.Else else_expr -> else_expr |> List.map (eval env) |> List.rev |> List.hd
  | Ast.Ident n  -> List.hd(get_env n env) 
  | Ast.SetVar (var,expr,expr2) -> let env2 = add_env (var |> to_string) [] [(eval env expr)] env in eval env2 expr2
  | Ast.SetFunc (func_name,args,expr) -> let env2 = 
                                           add_env (func_name |> to_string) args expr env in Ast.Env(env2)
  | Ast.CallFunc (func_name,args) -> try 
      let (v_args,body) = get_func (func_name |> to_string) env
      in let env1 = set_vargs  (List.map (fun x -> to_string x) v_args) (List.map (eval env) args) env in
      let r = List.map (eval env1) body in
      List.hd (List.rev r)
    with NameError _ -> let func = 
                          get_build_in_func (func_name |> to_string) build_in_func
      in 
      func (List.map (eval env) args)