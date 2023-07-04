open Ast
module Env = Map.Make (String)

type value = VInt of int | VBool of bool | VClosure of string * expr * env
and env = value Env.t

let err_unbnound_var = "Unbound variable"
let err_type_error = "Type error"

let rec eval env e =
  match e with
  | EInt i -> VInt i
  | EBool b -> VBool b
  | EFun (x, e) -> VClosure (x, e, env)
  | EVar x -> eval_var env x
  | EIf (e1, e2, e3) -> eval_if env e1 e2 e3
  | EApp (e1, e2) -> eval_app env e1 e2

and eval_var env x =
  try Env.find x env with Not_found -> failwith err_unbnound_var

and eval_if env e1 e2 e3 =
  match eval env e1 with
  | VBool b -> if b then eval env e2 else eval env e3
  | VInt _ | VClosure _ -> failwith err_type_error

and eval_app env e1 e2 =
  match eval env e1 with
  | VClosure (x, e, defenv) ->
      let v2 = eval env e2 in
      let defenv' = Env.add x v2 defenv in
      eval defenv' e
  | VInt _ | VBool _ -> failwith err_type_error
