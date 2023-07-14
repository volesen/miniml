{
open Parser
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let int = digit+
let id = alpha (alpha | digit | '_')*

let whitespace = [' ' '\t']+

rule read =
	parse
	| eof { EOF }
	| whitespace { read lexbuf }
	| '(' { LPAREN }
	| ')' { RPAREN }
	| '+' { PLUS }
	| '-' { MINUS }
	| '*' { STAR }
	| '=' { EQUALS }
	| "<=" { LTE }
	| "true" { TRUE }
	| "false" { FALSE }
	| "if" { IF }
	| "then" { THEN }
	| "else" { ELSE }
	| "let" { LET }
	| "in" { IN }
	| "fun" { FUN }
	| "->" { ARROW }
	| "rec" { REC }
	| int { INT (int_of_string (Lexing.lexeme lexbuf)) }
	| id { ID (Lexing.lexeme lexbuf) }
