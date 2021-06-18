%{
    open Ast
    open Util
%}

%token <int> INT
%token <string> STRING
%token <char> CHAR
%token <bool> BOOL
%token <float> FLOAT
%token ADD SUB MUL DIV MOD
%token EQ NOTEQ
%token LPAREN RPAREN
%token EOL
%left EQ  NOTEQ
%left ADD SUB
%left MUL DIV MOD
%nonassoc UMINUS

%start f
%type <Ast.expr> f
%%
f: expr EOL {$1}
expr :
    INT {Int $1}
    | STRING {String $1}
    | CHAR {Char $1}
    | BOOL {Bool $1}
    | FLOAT {Float $1}
    | LPAREN expr RPAREN {$2}
    | expr ADD expr {Add($1,$3)}
    | expr SUB expr {Sub($1,$3)}
    | expr MUL expr {Mul($1,$3)}
    | expr DIV expr {Div($1,$3)}
    | expr MOD expr {Mod($1,$3)}
    | SUB expr %prec UMINUS {
        match $2 with
        | Ast.Int _ -> Ast.Int(- (number $2))
        | Ast.Float _ -> Ast.Float(-. (float $2))
        | _ -> err("Parser Error")
        }
    | expr EQ expr {Eq($1,$3)}
    | expr NOTEQ expr {NotEq($1,$3)}
    | error { 
      let message =
        Printf.sprintf 
          "parse error near characters %d %d"
          (Parsing.symbol_start ())
	        (Parsing.symbol_end ())
	    in
	    failwith message
	  }
