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
  | If of expr * expr list * expr
  | Else of expr list
  | CallFunc of expr * expr list
  | SetFunc of expr * expr list * expr list
  | SetVar of expr * expr * expr
  | Ident of string
  | Env of env_filed list
and env_filed = {
  ident_name: string;
  args: expr list;
  body: expr list;
}