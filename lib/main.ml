open Ast
open Typing
open Eval

let parse (s : string): expr =
  s
  |> Lexing.from_string
  |> Parser.program Lexer.read

let interpret (s : string) : string =
  s
  |> parse
  |> typecheck
  |> eval Env.empty
  |> string_of_value

let repl () =
  let rec loop () =
    print_string "> ";
    let line = read_line () in
    try
      let result = interpret line in
      print_endline result;
      loop ()
    with Failure msg ->
      print_endline msg;
      loop ()
  in
  loop ()
