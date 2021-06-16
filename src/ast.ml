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