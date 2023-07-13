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

%nonassoc ELSE IN ARROW
%left LTE
%left PLUS MINUS
%left STAR
%nonassoc INT ID TRUE FALSE LPAREN FUN IF LET // Tokens that start an expression
%nonassoc APP // Only used for precedence

%start program

%type <Ast.expr>program
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
	| e1 = expr; e2 = expr %prec APP { EApp(e1, e2) }
	;

%inline bin_op:
	| PLUS { Add }
	| MINUS { Sub }
	| STAR { Mul }
	| LTE { Lte }
	;
