{
open Parser
}


let digit = ['0'-'9']
let number = '-'? digit digit*
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
    | eof {EOL}

