{
open Parser
open Char
}


let float =((['1'-'9']['0'-'9']*)|'0')('.'['0'-'9']['0'-'9']*)
let number =((['1'-'9']['0'-'9']*)|'0')
let char = (['0'-'9']|['a'-'z']|['A'-'Z']|' '|'"'|['\'' '\\' '+' '*' '/' '-' '=' '.' '<' '>' '(' ')' '{' '}' ';' '^' ':' '\'' '|' '[' ']' '^' '%' '$'])
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
    | "==" {EQ}
    | "!=" {NOTEQ}
    | "<" {LT}
    | "<=" {LE}
    | ">" {GT}
    | ">=" {GE}
    | "if" {IF}
    | "then" {THEN}
    | "else" {ELSE}
    | "true" {BOOL(true)}
    | "false" {BOOL(false)}
    | number as n {INT (int_of_string n)}
    | float as n {FLOAT (float_of_string n)}
    | "\"" {STRING(string lexbuf)}
    | "'"(char as c)"'" {CHAR(c)}
    | eof {EOL}

and string = parse
           | "\"" {""}
           | _ as s { escaped s ^ (string lexbuf)}

