open Ast

let parse (s: string): expr =
  s
  |> Lexing.from_string
  |> Parser.program Lexer.read
