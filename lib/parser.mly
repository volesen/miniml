%{
open Ast
%}

%token EOF
%token <int> INT
%token <string> ID
%token LPAREN
%token RPAREN
%token PLUS
%token MINUS
%token STAR
%token LTE
%token TRUE
%token FALSE

%left LTE
%left PLUS MINUS
%left STAR

%start <Ast.expr>program

%%

program:
	| e = expr; EOF { e }
	;

expr:
	| i = INT { EInt i }
	| id = ID { EVar id }
	| TRUE { EBool true }
	| FALSE { EBool false }
	| e1 = expr; op = bin_op; e2 = expr { EBinop(op, e1, e2) }
	| LPAREN; e = expr; RPAREN { e }
	;

%inline bin_op:
	| PLUS { Add }
	| MINUS { Sub }
	| STAR { Mul }
	| LTE { Lte }
	;
