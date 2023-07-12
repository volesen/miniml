open Ast

type typ = TInt | TBool | TVar of string | TArrow of typ * typ
type scheme = Forall of string list * typ

module VarSet = Set.Make (String)
module Ctx = Map.Make (String)

type ctx = scheme Ctx.t

let fresh_var =
  let counter = ref 0 in
  fun () ->
    incr counter;
    TVar ("x" ^ string_of_int !counter)

(** [subst x t' t] is [t] with [TVar x] substituted for [t']. *)
let rec subst x t' t =
  match t with
  | TInt | TBool -> t
  | TVar x' -> if x = x' then t' else t
  | TArrow (t1, t2) -> TArrow (subst x t' t1, subst x t' t2)

let apply_subs subs t =
  List.fold_left (fun acc (x, t') -> subst x t' acc) t subs

let apply_subs_scheme subs (Forall (xs, t)) = Forall (xs, apply_subs subs t)
let apply_subs_ctx subs ctx = Ctx.map (apply_subs_scheme subs) ctx

(** [occurs x t] returns true if the type variable [x] occurs in type [t]. *)
let rec occurs x t =
  match t with
  | TInt | TBool -> false
  | TVar x' -> x = x'
  | TArrow (t1, t2) -> occurs x t1 || occurs x t2

(** [unify cs] returns subtitutiouns that unifies the constraint set [cs]. *)
let rec unify cs = match cs with [] -> [] | c :: cs -> unify_step c cs

and unify_step c cs =
  match c with
  | TInt, TInt | TBool, TBool -> unify cs
  | TVar x, TVar y when x = y -> unify cs
  | TVar x, t | t, TVar x ->
      if occurs x t then failwith "Infinite type"
      else
        (* Substitute x for t in the rest of the constraints *)
        let cs' = List.map (fun (t1, t2) -> (subst x t t1, subst x t t2)) cs in
        (x, t) :: unify cs'
  | TArrow (t1, t2), TArrow (t1', t2') -> unify ((t1, t1') :: (t2, t2') :: cs)
  | _ -> failwith "Failed to unify"

let rec ftv_of_typ t =
  match t with
  | TInt | TBool -> VarSet.empty
  | TVar x -> VarSet.singleton x
  | TArrow (t1, t2) -> VarSet.union (ftv_of_typ t1) (ftv_of_typ t2)

let ftv_of_scheme (Forall (xs, t)) =
  VarSet.diff (ftv_of_typ t) (VarSet.of_list xs)

let ftv_of_ctx ctx =
  Ctx.fold (fun _ s acc -> VarSet.union (ftv_of_scheme s) acc) ctx VarSet.empty

(** [generalize cs ctx x t] returns a type scheme for [t] in context [ctx]
    where the free type variables in [ty] that are not bound in [env] are quantified. *)
let generalize cs ctx x t =
  let subs = unify cs in
  let t' = apply_subs subs t in
  let ctx' = apply_subs_ctx subs ctx in
  let free_vars =
    VarSet.diff (ftv_of_typ t') (ftv_of_ctx ctx') |> VarSet.elements
  in
  Ctx.add x (Forall (free_vars, t')) ctx'

(** [instantiate s] returns a type from the scheme [s] by replacing all
    quantified type variables with fresh type variables. *)
let instantiate (Forall (xs, t)) =
  let subs = List.map (fun x -> (x, fresh_var ())) xs in
  apply_subs subs t

(** [infer ctx e] returns the type and constraints of the expression [e] in
    context [ctx]. *)
let rec infer ctx e =
  match e with
  | EInt _ -> (TInt, [])
  | EBool _ -> (TBool, [])
  | EVar x -> infer_var ctx x
  | EBinop (op, e1, e2) -> infer_binop ctx op e1 e2
  | ELet (x, e1, e2) -> infer_let ctx x e1 e2
  | EIf (e1, e2, e3) -> infer_if ctx e1 e2 e3
  | EFun (x, e) -> infer_fun ctx x e
  | EApp (e1, e2) -> infer_app ctx e1 e2
  | ERec (x, e) -> infer_rec ctx x e

and infer_var ctx x =
  try
    let s = Ctx.find x ctx in
    let t = instantiate s in
    (t, [])
  with Not_found -> failwith ("Unbound variable " ^ x)

and infer_binop ctx op e1 e2 =
  let t1, c1 = infer ctx e1 in
  let t2, c2 = infer ctx e2 in
  match op with
  | Add ->
      let c = [ (t1, TInt); (t2, TInt) ] in
      (TInt, c1 @ c2 @ c)
  | Lte ->
      let c = [ (t1, TInt); (t2, TInt) ] in
      (TBool, c1 @ c2 @ c)

and infer_let ctx x e1 e2 =
  let t1, c1 = infer ctx e1 in
  let ctx' = generalize c1 ctx x t1 in
  let t2, c2 = infer ctx' e2 in
  (t2, c1 @ c2)

and infer_if ctx e1 e2 e3 =
  let t' = fresh_var () in
  let t1, c1 = infer ctx e1 in
  let t2, c2 = infer ctx e2 in
  let t3, c3 = infer ctx e3 in
  let c = [ (t1, TBool); (t', t2); (t', t3) ] in
  (t', c1 @ c2 @ c3 @ c)

and infer_fun ctx x e =
  let t1' = fresh_var () in
  let ctx' = Ctx.add x (Forall ([], t1')) ctx in
  let t2, c2 = infer ctx' e in
  (TArrow (t1', t2), c2)

and infer_app ctx e1 e2 =
  let t' = fresh_var () in
  let t1, c1 = infer ctx e1 in
  let t2, c2 = infer ctx e2 in
  let c = [ (t1, TArrow (t2, t')) ] in
  (t', c1 @ c2 @ c)

and infer_rec ctx x e =
  let t1' = fresh_var () in
  let env' = Ctx.add x (Forall ([], t1')) ctx in
  let t2, c2 = infer env' e in
  let c = [ (t1', t2) ] in
  (t1', c2 @ c)

let infer_top e =
  let t, cs = infer Ctx.empty e in
  let substs = unify cs in
  apply_subs substs t

let typecheck e =
  ignore (infer_top e);
  e
