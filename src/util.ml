open Char

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
  | Ast.String s -> s
  | Ast.Float f -> f |> string_of_float
  | Ast.Char c -> c |> escaped
  | _ -> err("Not to Display")

let add n m =
  match n with
  | Ast.Int _ -> Ast.Int (number n +  number m)
  | Ast.Float _ -> Ast.Float (float n +. float  m)
  | Ast.String _-> Ast.String (string n ^ string  m)
  | _ -> err("Not Called Add Operater")
let sub n m =
  match n with 
  | Ast.Int _ -> Ast.Int (number  n - number m)
  | Ast.Float _ -> Ast.Float (float n  -. float  m)
  | _ -> err("Not Colled Sub Operater")
let mul n m = 
  match n with 
  | Ast.Int _ -> Ast.Int (number n * number m)
  | Ast.Float _ ->Ast.Float (float n *. float m)
  | _ -> err("Not Colled Mul Operater")
let div n m = 
  match n with
  | Ast.Int _ -> let mm = number m in (
      match mm with 
      | 0 -> err("Division by zero")
      | _ -> Ast.Int (number  n / mm))
  | Ast.Float _ -> let mm = float m in (
      match mm with 
      | 0.0 -> err("Division by zero")
      | _ -> Ast.Float (float  n /. mm))
  | _ -> err("Not Colled Div Operater")
let modd n m = match n with
  | Ast.Int _ -> let mm = number m in (
      match mm with 
      | 0 -> err("Division by zero")
      | _ -> Ast.Int (number  n mod mm))
  | _ -> err("Not Colled Mod Operater")

let eq n m = match n with
| Ast.Bool _ -> Ast.Bool(bool n = bool m)
| Ast.Char _ -> Ast.Bool(char n = char m)
| Ast.String _ -> Ast.Bool(string n = string m)
| Ast.Int _ -> Ast.Bool(number n = number m)
| Ast.Float _ -> Ast.Bool(float n = float m)
| _ -> err("Not Colled Eq Operater")

let noteq n m = match n with
| Ast.Bool _ -> Ast.Bool(bool n <> bool m)
| Ast.Char _ -> Ast.Bool(char n <> char m)
| Ast.String _ -> Ast.Bool(string n <> string m)
| Ast.Int _ -> Ast.Bool(number n <> number m)
| Ast.Float _ -> Ast.Bool(float n <> float m)
| _ -> err("Not Colled NotEq Operater")