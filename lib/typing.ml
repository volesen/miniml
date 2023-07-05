open Ast

type typ = TInt | TBool | TVar of string | TArrow of typ * typ

module Ctx = Map.Make (String)

type ctx = typ Ctx.t

let fresh =
  let counter = ref 0 in
  fun () ->
    incr counter;
    TVar ("'x" ^ string_of_int !counter)

let rec infer ctx e =
  match e with
  | EInt _ -> (TInt, [])
  | EBool _ -> (TBool, [])
  | EVar x -> (Ctx.find x ctx, [])
  | EIf (e1, e2, e3) -> infer_if ctx e1 e2 e3
  | EFun (x, e) -> infer_fun ctx x e
  | EApp (e1, e2) -> infer_app ctx e1 e2

and infer_if ctx e1 e2 e3 =
  let t' = fresh () in
  let t1, c1 = infer ctx e1 in
  let t2, c2 = infer ctx e2 in
  let t3, c3 = infer ctx e3 in
  let c = [ (t1, TBool); (t', t2); (t', t3) ] in
  (t', c1 @ c2 @ c3 @ c)

and infer_fun ctx x e =
  let t1' = fresh () in
  let ctx' = Ctx.add x t1' ctx in
  let t2, c2 = infer ctx' e in
  (TArrow (t1', t2), c2)

and infer_app ctx e1 e2 =
  let t' = fresh () in
  let t1, c1 = infer ctx e1 in
  let t2, c2 = infer ctx e2 in
  let c = [ (t1, TArrow (t2, t')) ] in
  (t', c1 @ c2 @ c)

(** [subst x t' t] is [t] with [TVar x] substituted for [t].
    Formally t{x/t'}. *)
let rec subst x t' t =
  match t with
  | TInt | TBool -> t
  | TVar x' -> if x = x' then t' else t
  | TArrow (t1, t2) -> TArrow (subst x t' t1, subst x t' t2)

(** [csubst x t c] substitutes [TVar x] with [t] in
     the left and right hand side of the containt [c]. *)
let csubst x t (t1, t2) = (subst x t t1, subst x t t2)

(** [occurs x t] returns true if the type variable [x] occurs in type [t]. *)
let rec occurs x t =
  match t with
  | TInt | TBool -> false
  | TVar x' -> x = x'
  | TArrow (t1, t2) -> occurs x t1 || occurs x t2

let err_no_reductions = "No reductions can be applied in unification"

(** [unify cs] returns subtitutiouns that unifies the constraint set [cs]. *)
let rec unify cs = match cs with [] -> [] | c :: cs -> unify_step c cs

and unify_step c cs =
  match c with
  | TInt, TInt | TBool, TBool -> unify cs
  | TVar x, TVar y when x = y -> unify cs
  | TVar x, t | t, TVar x ->
      if not (occurs x t) then
        let cs' = List.map (csubst x t) cs in
        (x, t) :: unify cs'
      else failwith err_no_reductions
  | TArrow (t1, t2), TArrow (t1', t2') -> unify ((t1, t1') :: (t2, t2') :: cs)
  | _ -> failwith err_no_reductions

let typecheck e =
  ignore (e |> infer Ctx.empty |> snd |> unify);
  e

let rec string_of_typ = function
  | TInt -> "int"
  | TBool -> "bool"
  | TVar x -> x
  | TArrow (t1, t2) -> "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"
