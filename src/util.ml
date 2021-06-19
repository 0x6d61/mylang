open Char
open Syntax

let to_string expr = match expr with
  | Ast.Bool b -> b |> string_of_bool
  | Ast.Int i -> i |> string_of_int
  | Ast.String s -> s
  | Ast.Float f -> f |> string_of_float
  | Ast.Char c -> c |> escaped
  | Ast.Ident i -> i
  | _ -> err("Not to Display")

let print expr = (
  if List.length expr = 0 then
      err("TypeError: expected print "^ (List.length expr |> string_of_int) ^ " arguments got 1")
  else
    print_string (to_string (List.hd expr));
    print_newline ();
    Ast.Int(-1)
)