open Ast

type typ = TInt | TBool | TVar of string | TArrow of typ * typ
and scheme = Forall of string list * typ

module VarSet = Set.Make (String)
module Ctx = Map.Make (String)

type ctx = scheme Ctx.t

(** [bind x t] binds the type [t] to the *)
let bind x t env = Ctx.add x (Forall ([], t)) env

(** [tv t] is the type variables of [t]. *)
let rec tv_of_t t =
  match t with
  | TInt | TBool -> VarSet.empty
  | TVar x -> VarSet.singleton x
  | TArrow (t1, t2) -> VarSet.union (tv_of_t t1) (tv_of_t t2)

let tv_of_ctx (ctx : ctx) =
  Ctx.fold (fun x _ acc -> VarSet.add x acc) ctx VarSet.empty

(** [occurs x t] returns true if the type variable [x] occurs in type [t]. *)
let rec occurs x t =
  match t with
  | TInt | TBool -> false
  | TVar x' -> x = x'
  | TArrow (t1, t2) -> occurs x t1 || occurs x t2

(** [subst x t' t] is [t] with [TVar x] substituted for [t].
    Formally t{x/t'}. *)
let rec subst x t' t =
  match t with
  | TInt | TBool -> t
  | TVar x' -> if x = x' then t' else t
  | TArrow (t1, t2) -> TArrow (subst x t' t1, subst x t' t2)

(** [csubst x t c] substitutes [TVar x] with [t] in
     the left and right hand side of the constraint [c]. *)
let csubst x t (t1, t2) = (subst x t t1, subst x t t2)

let apply_substs t s = List.fold_left (fun acc (x, t) -> subst x t acc) t s
let err_unbound_var = "Unbound variable"
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

(** [fresh ()] returns a fresh type variable. *)
let fresh =
  let counter = ref 0 in
  fun () ->
    incr counter;
    TVar ("x" ^ string_of_int !counter)

(** [generalize cs ctx x t] *)
let generalize cs ctx (x : string) (t : typ) : ctx =
  (* Step 1: Pretend e1 is the entire program*)
  let s = unify cs in
  let t' = apply_substs t s in
  let ctx' =
    Ctx.map (fun (Forall (xs, t)) -> Forall (xs, apply_substs t s)) ctx
  in
  (* Step 2: Remove all type variables that are in the context *)
  let xs = VarSet.diff (tv_of_t t') (tv_of_ctx ctx') in
  (* Step 3: Generalize *)
  Ctx.add x (Forall (VarSet.elements xs, t')) ctx'

(** [instantiate xs t] substitues the type variables [xs]
    with fresh type variables in [t] *)
let instantiate xs t : typ =
  List.fold_left (fun acc x -> subst x (fresh ()) acc) t xs

let rec infer (ctx : ctx) (e : expr) : typ * (typ * typ) list =
  match e with
  | EInt _ -> (TInt, [])
  | EBool _ -> (TBool, [])
  | EVar x -> infer_var ctx x
  | ELet (x, e1, e2) -> infer_let ctx x e1 e2
  | EIf (e1, e2, e3) -> infer_if ctx e1 e2 e3
  | EFun (x, e) -> infer_fun ctx x e
  | EApp (e1, e2) -> infer_app ctx e1 e2

and infer_var ctx x =
  try
    let (Forall (xs, t)) = Ctx.find x ctx in
    (instantiate xs t, [])
  with Not_found -> failwith err_unbound_var

and infer_let ctx x e1 e2 =
  let t1, c1 = infer ctx e1 in
  let ctx' = generalize c1 ctx x t1 in
  let t2, c2 = infer ctx' e2 in
  (t2, c1 @ c2)

and infer_if ctx e1 e2 e3 =
  let t' = fresh () in
  let t1, c1 = infer ctx e1 in
  let t2, c2 = infer ctx e2 in
  let t3, c3 = infer ctx e3 in
  let c = [ (t1, TBool); (t', t2); (t', t3) ] in
  (t', c1 @ c2 @ c3 @ c)

and infer_fun ctx x e =
  let t1 = fresh () in
  (* TODO: Cleanup the following line.
     let s1 = Forall ([], t1) in
     let ctx' = Ctx.add x s1 ctx in
  *)
  let ctx' = bind x t1 ctx in
  let t2, c2 = infer ctx' e in
  (TArrow (t1, t2), c2)

and infer_app ctx e1 e2 =
  let t' = fresh () in
  let t1, c1 = infer ctx e1 in
  let t2, c2 = infer ctx e2 in
  let c = [ (t1, TArrow (t2, t')) ] in
  (t', c1 @ c2 @ c)

let typecheck e =
  ignore (e |> infer Ctx.empty |> snd |> unify);
  e

let infer_top e =
  let t, cs = infer Ctx.empty e in
  let s = unify cs in
  let t' = List.fold_left (fun t (x, t') -> subst x t' t) t s in
  t'
