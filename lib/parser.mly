%{
open Ast
%}

%token EOF
%token <int> INT
%token <string> ID
%token LPAREN RPAREN
%token PLUS MINUS STAR EQUALS LTE
%token TRUE FALSE
%token IF THEN ELSE
%token LET IN
%token FUN ARROW
%token REC

%nonassoc ELSE IN ARROW
%left LTE
%left PLUS MINUS
%left STAR

%start program

%type <Ast.expr> program expr unary app primary

%%

program:
	| e = expr; EOF { e }
	;

expr:
	| e1 = expr; op = bin_op; e2 = expr { EBinOp(op, e1, e2) }
	| IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { EIf(e1, e2, e3) }
	| LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { ELet(x, e1, e2) }
	| FUN; x = ID; ARROW; e = expr { EFun(x, e) }
	| REC; x = ID; ARROW; e = expr { ERec(x, e) }
	| e = unary { e }
	;

%inline bin_op:
	| PLUS { Add }
	| MINUS { Sub }
	| STAR { Mul }
	| LTE { Lte }
	;

unary:
	| op = un_op; e = unary { EUnOp(op, e) }
	| e = app { e }
	;

%inline un_op:
	| MINUS { Neg }


app:
	| e1 = app; e2 = primary { EApp(e1, e2) }
	| e = primary { e }
	;

primary:
	| i = INT { EInt i }
	| id = ID { EVar id }
	| TRUE { EBool true }
	| FALSE { EBool false }
	| LPAREN; e = expr; RPAREN { e }
	;
