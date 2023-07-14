type un_op = Neg

type bin_op =
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
  | EBinOp of bin_op * expr * expr
  | ELet of string * expr * expr
  | EIf of expr * expr * expr
  | EApp of expr * expr
  | EFun of string * expr
  | ERec of string * expr
