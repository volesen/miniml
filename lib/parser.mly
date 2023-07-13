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
%token EQUALS
%token LTE
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token LET
%token IN
%token FUN
%token ARROW

%nonassoc ARROW
%nonassoc ELSE
%nonassoc IN
%left LTE
%left PLUS MINUS
%left STAR
%nonassoc INT ID TRUE FALSE LPAREN IF LET FUN /* ALL other tokens that start an expr */
%nonassoc APP

%start <Ast.expr>program

%type <Ast.expr>expr;

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
	| IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { EIf(e1, e2, e3) }
	| LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { ELet(x, e1, e2) }
	| LPAREN; e = expr; RPAREN { e }
	| FUN; x = ID; ARROW; e = expr { EFun(x, e) }
	| e1 = expr; e2 = expr { EApp(e1, e2) } %prec APP
	;

%inline bin_op:
	| PLUS { Add }
	| MINUS { Sub }
	| STAR { Mul }
	| LTE { Lte }
	;
