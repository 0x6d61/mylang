%{
    open Ast
%}

//基本的な値
%token <int> INT
%token <string> STRING
%token <char> CHAR
%token <bool> BOOL
%token <float> FLOAT
%token <string> IDENT

// 演算子
%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token EQ 
%token NOTEQ
%token LT
%token LE 
%token GT 
%token GE
//記号
%token RET
%token ALLOW
%token COMMA
//括弧
%token LPAREN
%token RPAREN
%token LBRACES
%token RBRACES 
//予約語

%token FN
%token IN
%token IF
%token THEN
%token ELSE

%token EOF


%right prec_if
%left EQ NOTEQ LT LE GT GE
%left ADD SUB
%left MUL DIV MOD
%nonassoc UMINUS

%start f
%type <Ast.expr list> f

%%

f: expr* EOF {$1}

var: IDENT {Ident($1)}

params:
    | expr COMMA params
        {$1 :: $3}
    | expr
         {[$1]}

func_params:
    | var COMMA func_params {$1 :: $3}
    | var {[$1]}

expr:
    | var {$1}
    | LPAREN expr RPAREN {$2}
    | INT {Int $1}
    | STRING {String $1}
    | CHAR {Char $1}
    | BOOL {Bool $1}
    | FLOAT {Float $1}
    | expr ADD expr {Add($1,$3)}
    | expr SUB expr {Sub($1,$3)}
    | expr MUL expr {Mul($1,$3)}
    | expr DIV expr {Div($1,$3)}
    | expr MOD expr {Mod($1,$3)}
    | SUB expr %prec UMINUS { Uminus($2) }
    | expr EQ expr {Eq($1,$3)}
    | expr NOTEQ expr {NotEq($1,$3)}
    | expr LT expr {Lt($1,$3)}
    | expr LE expr {Le($1,$3)}
    | expr GT expr {Gt($1,$3)}
    | expr GE expr {Ge($1,$3)} 
    | IF e1 = expr THEN e2 = expr ELSE e3 = expr %prec prec_if {
        If(e1,e2,e3)
    }
    | var RET expr IN expr {SetVar($1,$3,$5)}
    | var LPAREN params RPAREN {CallFunc ($1,$3)}
    | var LPAREN RPAREN {CallFunc($1,[])}
    | FN v = var LPAREN p=func_params RPAREN ALLOW LBRACES e=expr* RBRACES {SetFunc(v,p,e)}
    | FN v = var LPAREN RPAREN ALLOW LBRACES e=expr* RBRACES {SetFunc(v,[],e)}
    | error { 
      let message =
        Printf.sprintf 
          "parse error near characters %d %d"
          (Parsing.symbol_start ())
	        (Parsing.symbol_end ())
	    in
	    failwith message
	  }