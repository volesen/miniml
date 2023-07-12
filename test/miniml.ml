open Miniml.Ast
open Miniml.Typing
open Miniml.Eval
open Miniml.Parse

let sum_to =
  ERec
    ( "sum_to",
      EFun
        ( "n",
          EIf
            ( EBinop (Lte, EVar "n", EInt 0),
              EInt 0,
              EBinop
                ( Add,
                  EVar "n",
                  EApp (EVar "sum_to", EBinop (Sub, EVar "n", EInt 1)) ) ) ) )

let testable_expr =
  let rec pp_expr pp e = Fmt.pf pp "%a" pp_expr e in
  Alcotest.testable pp_expr ( = )

let test_parse_true () =
  Alcotest.(check testable_expr) "true" (EBool true) (parse "true")

let test_parse_false () =
  Alcotest.(check testable_expr) "false" (EBool false) (parse "false")

let test_parse_int () =
  Alcotest.(check testable_expr) "42" (EInt 42) (parse "42")

let test_parse_neg_int () =
  Alcotest.(check testable_expr) "-42" (EInt (-42)) (parse "-42")

let test_parse_add () =
  Alcotest.(check testable_expr)
    "1 + 2"
    (EBinop (Add, EInt 1, EInt 2))
    (parse "1 + 2")

let test_parse_sub () =
  Alcotest.(check testable_expr)
    "1 - 2"
    (EBinop (Sub, EInt 1, EInt 2))
    (parse "1 - 2")

let test_parse_mul () =
  Alcotest.(check testable_expr)
    "1 * 2"
    (EBinop (Mul, EInt 1, EInt 2))
    (parse "1 * 2")

let test_parse_lte () =
  Alcotest.(check testable_expr)
    "1 <= 2"
    (EBinop (Lte, EInt 1, EInt 2))
    (parse "1 <= 2")

let test_parse_assoc () =
  Alcotest.(check testable_expr)
    "1 + 2 + 3"
    (EBinop (Add, EBinop (Add, EInt 1, EInt 2), EInt 3))
    (parse "1 + 2 + 3")

let test_parse_prec () =
  Alcotest.(check testable_expr)
    "1 + 2 * 3"
    (EBinop (Add, EInt 1, EBinop (Mul, EInt 2, EInt 3)))
    (parse "1 + 2 * 3")

let test_parse_if () =
  Alcotest.(check testable_expr)
    "if true then 1 else 2"
    (EIf (EBool true, EInt 1, EInt 2))
    (parse "if true then 1 else 2")

let test_parse_if_assoc () =
  Alcotest.(check testable_expr)
    "if true then 1 else if false then 2 else 3"
    (EIf (EBool true, EInt 1, EIf (EBool false, EInt 2, EInt 3)))
    (parse "if true then 1 else if false then 2 else 3")

let test_parse_if_prec () =
  Alcotest.(check testable_expr)
    "if true then 1 else 2 + 3"
    (EIf (EBool true, EInt 1, EBinop (Add, EInt 2, EInt 3)))
    (parse "if true then 1 else 2 + 3")

let test_parse_let () =
  Alcotest.(check testable_expr)
    "let x = 1 in x + 2"
    (ELet ("x", EInt 1, EBinop (Add, EVar "x", EInt 2)))
    (parse "let x = 1 in x + 2")

let test_parse_let_prec () =
  Alcotest.(check testable_expr)
    "let x = 1 in x + 2 * 3"
    (ELet ("x", EInt 1, EBinop (Add, EVar "x", EBinop (Mul, EInt 2, EInt 3))))
    (parse "let x = 1 in x + 2 * 3")

let test_parse_let_assoc () =
  Alcotest.(check testable_expr)
    "let x = 1 in let y = 2 in x + y"
    (ELet ("x", EInt 1, ELet ("y", EInt 2, EBinop (Add, EVar "x", EVar "y"))))
    (parse "let x = 1 in let y = 2 in x + y")

let testable_value =
  let pp_value pp v =
    match v with
    | VInt i -> Fmt.pf pp "%d" i
    | VBool b -> Fmt.pf pp "%b" b
    | VClosure _ -> Fmt.pf pp "<fun>"
  in
  Alcotest.testable pp_value ( = )

let testable_typ =
  let rec pp_typ pp t = Fmt.pf pp "%a" pp_typ t in
  Alcotest.testable pp_typ ( = )

let test_infer () =
  let ty = infer_top sum_to in
  Alcotest.(check testable_typ) "sum_to" (TArrow (TInt, TInt)) ty

let test_eval () =
  let env = Env.empty in
  let v = eval env (EApp (sum_to, EInt 10)) in
  Alcotest.(check testable_value) "sum_to 10" (VInt 55) v

let () =
  Alcotest.run "miniml"
    [
      ( "parse",
        [
          Alcotest.test_case "true" `Quick test_parse_true;
          Alcotest.test_case "false" `Quick test_parse_false;
          Alcotest.test_case "int" `Quick test_parse_int;
          Alcotest.test_case "negative integers" `Quick test_parse_neg_int;
          Alcotest.test_case "add" `Quick test_parse_add;
          Alcotest.test_case "sub" `Quick test_parse_sub;
          Alcotest.test_case "mul" `Quick test_parse_mul;
          Alcotest.test_case "less than equal" `Quick test_parse_lte;
          Alcotest.test_case "associativity" `Quick test_parse_assoc;
          Alcotest.test_case "precedence" `Quick test_parse_prec;
          Alcotest.test_case "if" `Quick test_parse_if;
          Alcotest.test_case "if precedence" `Quick test_parse_if_prec;
          Alcotest.test_case "if associativity" `Quick test_parse_if_assoc;
          Alcotest.test_case "let" `Quick test_parse_let;
          Alcotest.test_case "let precedence" `Quick test_parse_let_prec;
          Alcotest.test_case "let associativity" `Quick test_parse_let_assoc;
        ] );
      ("typing", [ Alcotest.test_case "infer" `Quick test_infer ]);
      ("eval", [ Alcotest.test_case "eval" `Quick test_eval ]);
    ]
