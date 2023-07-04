type expr =
  | EInt of int
  | EBool of bool
  | EVar of string
  | EIf of expr * expr * expr
  | EApp of expr * expr
  | EFun of string * expr
