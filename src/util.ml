open Char
open Syntax

let to_string expr = match expr with
  | Ast.Bool b -> b |> string_of_bool
  | Ast.Int i -> i |> string_of_int
  | Ast.String s -> s
  | Ast.Float f -> f |> string_of_float
  | Ast.Char c -> c |> escaped
  | _ -> err("Not to Display")

