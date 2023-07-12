type binop =
  (* For now we skip `eq` as it requires typeclasses *)
  | Add
  | Lt

type expr =
  | EInt of int
  | EBool of bool
  | EBinop of binop * expr * expr
  | EVar of string
  | ELet of string * expr * expr
  | EIf of expr * expr * expr
  | EApp of expr * expr
  | EFun of string * expr
