exception NameError of string
exception TypeError of string
exception Error of string
let err s = raise (Error s)
let name_err s = raise (NameError s)
let type_err s = raise (TypeError s)