open Syntax
open Util

type env_filed = {
  ident_name: string;
  args: Ast.expr list;
  body: Ast.expr;
}

(* env_filed list *)
let env = []
let build_in_func = [
    ("print",print);
]

let add_env name args body env =  {ident_name = name;args = args;body = body;} :: env

let set_vargs args body env =
  if List.length args <> List.length body then
    err("TypeError: expected "^ (List.length args |> string_of_int) ^ "arguments got " ^ (List.length body |> string_of_int))
  else
    let rec func arg by = if List.length arg = 0 then
        env
      else
        {ident_name = (List.hd arg) ; args = [];body = (List.hd by)} ::  func (List.tl arg) (List.tl by)
    in func args body 

let rec get_env var env =
  match env with
  | [] -> err("NameError: name  '" ^ var ^ "' is not defined")
  | x::xs -> if x.ident_name = var then
      x.body
    else
      get_env var xs

let rec get_func var env = 
    match env with 
  | [] -> err("NameError: name  '" ^ var ^ "' is not defined")
  | x::xs -> if x.ident_name = var then
      (x.args,x.body)
    else
      get_func var xs

let get_build_in_func func_name list =
    let rec func lst = match lst with
        | [] -> err("NameError: name  '" ^ func_name ^ "' is not defined")
        | (f,body)::fs -> if f = func_name then
                            body
                        else 
                            func fs
        in func list