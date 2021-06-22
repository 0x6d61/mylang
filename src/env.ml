open Error
open Util
open Ast

(* env_filed list ref*)
let env :Ast.env_filed list = []
let build_in_func = [
    ("print",print);
]

let add_env name args body env =  {ident_name = name;args = args;body = body;} :: env

(* 仮引数に実引数を割当 *)
let set_vargs vargs args env =
  if List.length vargs <> List.length args then
    type_err("TypeError: expected "^ (List.length vargs |> string_of_int) ^ " arguments got " ^ (List.length args |> string_of_int))
  else
    let rec func varg arg = if List.length varg = 0 then
        env
      else
        {ident_name = (List.hd varg) ; args = [];body = [(List.hd arg)]} ::  func (List.tl varg) (List.tl arg)
    in func vargs args


let rec get_env var env =
  match env with
  | [] -> name_err("NameError: name '" ^ var ^ "' is not defined")
  | x::xs -> if x.ident_name = var then
      x.body
    else
      get_env var xs

let rec get_func var env = 
    match env with 
  | [] -> name_err("NameError: name '" ^ var ^ "' is not defined")
  | x::xs -> if x.ident_name = var then
      (x.args,x.body)
    else
      get_func var xs

let get_build_in_func func_name list =
    let rec func lst = match lst with
        | [] -> name_err("NameError: name '" ^ func_name ^ "' is not defined")
        | (f,body)::fs -> if f = func_name then
                            body
                        else 
                            func fs
        in func list