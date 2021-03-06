{
open Parser
open Char
}


let float =((['1'-'9']['0'-'9']*)|'0')('.'['0'-'9']['0'-'9']*)
let number =((['1'-'9']['0'-'9']*)|'0')
let whitespace = ['\t' ' ' '\r']
let ident = ['a'-'z' 'A'-'Z' '_']  ['A'-'Z' 'a'-'z' '_' '0'-'9']*
rule tokenize = parse
    | whitespace+ {tokenize lexbuf}
    | "\n" { Lexing.new_line lexbuf; tokenize lexbuf}
    | "(" {LPAREN}
    | ")" {RPAREN}
    | "{" {LBRACES}
    | "}" {RBRACES}
    | "=" {RET}
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
    | "->" {ALLOW}
    | "," {COMMA}
    | "let" {LET}
    | "fn" {FN}
    | "if" {IF}
    | "in" {IN}
    | "else" {ELSE}
    | "true" {BOOL(true)}
    | "false" {BOOL(false)}
    | number as n {INT (int_of_string n)}
    | float as n {FLOAT (float_of_string n)}
    | "\"" {STRING(string lexbuf)}
    | ident {IDENT (Lexing.lexeme lexbuf)}
    | eof {EOF}

and string = parse
           | "\"" {""}
           | _ as s { escaped s ^ (string lexbuf)}

