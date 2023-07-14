open Ast
module Env = Map.Make (String)

type value = VInt of int | VBool of bool | VClosure of string * expr * env
and envbinding = value option ref
and env = envbinding Env.t

let string_of_value (v : value) : string =
  match v with
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VClosure _ -> "<fun>"

let err_unbnound_var = "Unbound variable"
let err_type_error = "Type error"
let err_bad_recursion = "Right hand side of `let rec` has to be a function"

let rec eval env e =
  match e with
  | EInt i -> VInt i
  | EBool b -> VBool b
  | EFun (x, e) -> VClosure (x, e, env)
  | EVar x -> eval_var env x
  | EUnOp (op, e) -> eval_un_op env op e
  | EBinOp (op, e1, e2) -> eval_bin_op env op e1 e2
  | ELet (x, e1, e2) -> eval_let env x e1 e2
  | EIf (e1, e2, e3) -> eval_if env e1 e2 e3
  | EApp (e1, e2) -> eval_app env e1 e2
  | ERec (x, e) -> eval_rec env x e

and eval_var env x =
  try
    match !(Env.find x env) with
    | Some v -> v
    | None -> failwith err_bad_recursion
  with Not_found -> failwith err_unbnound_var

and eval_un_op env op e =
  match (op, eval env e) with
  | Neg, VInt i -> VInt (-i)
  | _ -> failwith err_type_error

and eval_bin_op env op e1 e2 =
  match (op, eval env e1, eval env e2) with
  | Add, VInt i, VInt j -> VInt (i + j)
  | Sub, VInt i, VInt j -> VInt (i - j)
  | Mul, VInt i, VInt j -> VInt (i * j)
  | Lte, VInt i, VInt j -> VBool (i <= j)
  | _ -> failwith err_type_error

and eval_let env x e1 e2 =
  let v1 = eval env e1 in
  let env' = Env.add x (ref (Some v1)) env in
  eval env' e2

and eval_if env e1 e2 e3 =
  match eval env e1 with
  | VBool b -> if b then eval env e2 else eval env e3
  | VInt _ | VClosure _ -> failwith err_type_error

and eval_app env e1 e2 =
  match eval env e1 with
  | VClosure (x, e, defenv) ->
      let v2 = eval env e2 in
      let defenv' = Env.add x (ref (Some v2)) defenv in
      eval defenv' e
  | VInt _ | VBool _ -> failwith err_type_error

and eval_rec env x e =
  let v' = ref None in
  let env' = Env.add x v' env in
  let v = eval env' e in
  (* Tying the recursive knot *)
  v' := Some v;
  v
