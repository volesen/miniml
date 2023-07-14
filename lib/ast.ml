type un_op = Neg

type binop =
  (* For now we skip `eq` as it requires typeclasses *)
  | Add
  | Sub
  | Mul
  | Lte

type expr =
  | EInt of int
  | EBool of bool
  | EVar of string
  | EUnOp of un_op * expr
  | EBinOp of binop * expr * expr
  | ELet of string * expr * expr
  | EIf of expr * expr * expr
  | EApp of expr * expr
  | EFun of string * expr
  | ERec of string * expr
