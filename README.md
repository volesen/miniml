# miniml

A minim(a)l subset of OCaml with Hindley-Milner type inference.

## Example

```
let rec sum_to =
  fun n ->
    if n <= 0
      then 0
      else n + sum_to (n - 1)
in
  sum_to 10

> 55
```

## Quickstart

```bash
$ dune exec miniml
```

## Non-goals

- Pattern matching (for now)
- Good error messages

## Progress

- [x] Lexer
- [x] Parser
- [x] Sweet syntax
- [x] Basic interpreter
- [x] Recursion
- [ ] Binding groups
- [x] Type inference with let-polymorphism
- [x] REPL

## References

- [The Hindley-Milner Type System](https://cs3110.github.io/textbook/chapters/interp/inference.html)
- [Recursive closures](https://jerrington.me/posts/2023-01-20-recursive-closures.html)
- [Lexing and parsing with OCamllex and Menhir](https://mukulrathi.com/create-your-own-programming-language/parsing-ocamllex-menhir/)
- [Precedence in Menhir](https://ptival.github.io/2017/05/16/parser-generators-and-function-application/)