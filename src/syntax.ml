open Error

let err s = raise (Error s)

let number value = match value with
  | Ast.Int n -> n
  |_ -> type_err("TypeError: Not Integer")

let bool value = match value with
  | Ast.Bool n -> n
  | _ -> type_err("TypeError: Not Boolean")

let string value = match value with
  | Ast.String n -> n
  | _ -> type_err("TypeError: Not String")
let char value = match value with
  | Ast.Char n -> n
  | _ -> type_err("TypeError: Not Char")

let float value = match value with
  | Ast.Float n -> n
  | _ -> type_err("TypeError: Not Float")

  let add n m =
  match n with
  | Ast.Int _ -> Ast.Int (number n +  number m)
  | Ast.Float _ -> Ast.Float (float n +. float  m)
  | Ast.String _-> Ast.String (string n ^ string  m)
  | _ -> err("Not Called + Operater")
let sub n m =
  match n with 
  | Ast.Int _ -> Ast.Int (number  n - number m)
  | Ast.Float _ -> Ast.Float (float n  -. float  m)
  | _ -> err("Not Colled - Operater")
let mul n m = 
  match n with 
  | Ast.Int _ -> Ast.Int (number n * number m)
  | Ast.Float _ ->Ast.Float (float n *. float m)
  | _ -> err("Not Colled * Operater")
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
  | _ -> err("Not Colled / Operater")
let modd n m = match n with
  | Ast.Int _ -> let mm = number m in (
      match mm with 
      | 0 -> err("Division by zero")
      | _ -> Ast.Int (number  n mod mm))
  | _ -> err("Not Colled % Operater")

let eq n m = match n with
| Ast.Bool _ -> Ast.Bool(bool n = bool m)
| Ast.Char _ -> Ast.Bool(char n = char m)
| Ast.String _ -> Ast.Bool(string n = string m)
| Ast.Int _ -> Ast.Bool(number n = number m)
| Ast.Float _ -> Ast.Bool(float n = float m)
| _ -> err("Not Colled == Operater")

let noteq n m = match n with
| Ast.Bool _ -> Ast.Bool(bool n <> bool m)
| Ast.Char _ -> Ast.Bool(char n <> char m)
| Ast.String _ -> Ast.Bool(string n <> string m)
| Ast.Int _ -> Ast.Bool(number n <> number m)
| Ast.Float _ -> Ast.Bool(float n <> float m)
| _ -> err("Not Colled != Operater")

let lt n m = match n with
| Ast.Bool _ -> Ast.Bool(bool n < bool m)
| Ast.Char _ -> Ast.Bool(char n < char m)
| Ast.String _ -> Ast.Bool(string n < string m)
| Ast.Int _ -> Ast.Bool(number n < number m)
| Ast.Float _ -> Ast.Bool(float n < float m)
| _ -> err("Not Colled < Operater")

let le n m = match n with
| Ast.Bool _ -> Ast.Bool(bool n <= bool m)
| Ast.Char _ -> Ast.Bool(char n <= char m)
| Ast.String _ -> Ast.Bool(string n <= string m)
| Ast.Int _ -> Ast.Bool(number n <= number m)
| Ast.Float _ -> Ast.Bool(float n <= float m)
| _ -> err("Not Colled <= Operater")

let gt n m = match n with
| Ast.Bool _ -> Ast.Bool(bool n > bool m)
| Ast.Char _ -> Ast.Bool(char n > char m)
| Ast.String _ -> Ast.Bool(string n > string m)
| Ast.Int _ -> Ast.Bool(number n > number m)
| Ast.Float _ -> Ast.Bool(float n > float m)
| _ -> err("Not Colled > Operater")

let ge n m = match n with
| Ast.Bool _ -> Ast.Bool(bool n >= bool m)
| Ast.Char _ -> Ast.Bool(char n >= char m)
| Ast.String _ -> Ast.Bool(string n >= string m)
| Ast.Int _ -> Ast.Bool(number n >= number m)
| Ast.Float _ -> Ast.Bool(float n >= float m)
| _ -> err("Not Colled > Operater")

let iff expr  = match expr with
    | Ast.Bool _ -> bool expr
    | _ -> err("Not Colled if")
