{
open Parser
}


let float = '-'? ((['1'-'9']['0'-'9']*)|'0')('.'['0'-'9']['0'-'9']*)
let number = '-' ? ((['1'-'9']['0'-'9']*)|'0')
let whitespace = ['\t' ' ' '\n' '\r']
rule tokenize = parse
    | whitespace+ {tokenize lexbuf}
    | "(" {LPAREN}
    | ")" {RPAREN}
    | "+" {ADD}
    | "-" {SUB}
    | "*" {MUL}
    | "/" {DIV}
    | "%" {MOD}
    | number as n {INT (int_of_string n)}
    | float as n {FLOAT (float_of_string n)}
    | eof {EOL}

