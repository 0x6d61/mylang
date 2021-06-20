type expr = 
  | Int of int
  | String of string
  | Char of char
  | Bool of bool
  | Float of float
  | Uminus of expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | Eq of expr * expr
  | NotEq of expr * expr
  | Lt of expr * expr
  | Le of expr * expr
  | Gt of expr* expr
  | Ge of expr * expr
  | If of expr * expr * expr
  | CallFunc of expr * expr list
  | SetFunc of expr * expr list * expr
  | SetVar of expr * expr * expr
  | Env of string list
  | Ident of string