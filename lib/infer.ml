open DBType
open Subst
open Unify
open State
open TypeCtx

exception InferException of string

let returnNormalized (sub, typ) =
  let open IntState in
  return (sub, normalize typ)

let rec inferType' check (ctx : typeCtx) (e : Debruijn.expr) :
    (substitution * typeKind) IntState.t =
  let _ = check e in
  let open IntState in
  match e with
  | Int _ -> returnNormalized (emptySubst, Mono Int)
  | Bool _ -> returnNormalized (emptySubst, Mono Bool)
  | Unit -> returnNormalized (emptySubst, Mono Unit)
  | Var x ->
      let polyType : typeKind = find x ctx in
      returnNormalized (emptySubst, polyType)
  | Fun f ->
      freshName >>= fun x ->
      let newCtx = updateCtx ctx (Mono (FreshVar x)) in
      inferType' check newCtx f >>= fun (s, t) ->
      let tk = applySubstToTypeKind s (Mono (FreshVar x)) in
      returnNormalized (s, Rho (F (typeKindToPoly tk, typeKindToPoly t)))
  | App (f, g) ->
      inferType' check ctx f >>= fun (s1, tf) ->
      let newCtx = applySubstToCtx s1 ctx in
      inferType' check newCtx g >>= fun (s2, tg) ->
      freshName >>= fun x ->
      let tx =
        normalize
          (Rho (F (typeKindToPoly tg, typeKindToPoly (Mono (FreshVar x)))))
      in
      unify [ (tx, applySubstToTypeKind s2 tf) ] >>= fun s3 ->
      returnNormalized
        ( combineSubst s3 (combineSubst s2 s1),
          applySubstToTypeKind s3 (Mono (FreshVar x)) )
  | Pair (e1, e2) ->
      inferType' check ctx e1 >>= fun (s1, t1) ->
      let newCtx = applySubstToCtx s1 ctx in
      inferType' check newCtx e2 >>= fun (s2, t2) ->
      let s3 = combineSubst s2 s1 in
      let m1 = applySubstToTypeKind s3 t1 in
      let m2 = applySubstToTypeKind s3 t2 in
      returnNormalized (s3, Rho (P (typeKindToPoly m1, typeKindToPoly m2)))
  | (Fst e | Snd e) as e1 ->
      inferType' check ctx e >>= fun (s1, t1) ->
      freshName >>= fun x ->
      freshName >>= fun y ->
      let tx =
        normalize
          (Rho
             (P
                ( typeKindToPoly (Mono (FreshVar x)),
                  typeKindToPoly (Mono (FreshVar y)) )))
      in
      unify [ (tx, t1) ] >>= fun s2 ->
      let s3 = combineSubst s2 s1 in
      returnNormalized
        ( s3,
          applySubstToTypeKind s3
            (match e1 with
            | Fst _ -> Mono (FreshVar x)
            | _ -> Mono (FreshVar y)) )
  | TypeApp (e, t) ->
      inferType' check ctx e >>= fun (s1, t1) ->
      freshName >>= fun x ->
      let tx = Poly (1, T (FreshVar x)) in
      unify [ (tx, t1) ] >>= fun s2 ->
      let s3 = combineSubst s2 s1 in
      let tw =
        DBType.substType t 0 (applySubstToTypeKind s3 (Mono (FreshVar x)))
      in
      returnNormalized (s3, tw)
  | FunType (t, f) ->
      let newCtx = updateCtx ctx t in
      inferType' check newCtx f >>= fun (s1, t1) ->
      let m1 = applySubstToTypeKind s1 t in
      returnNormalized (s1, Rho (F (typeKindToPoly m1, typeKindToPoly t1)))
  | Lam e ->
      inferType' check (shift 1 0 ctx) e >>= fun (s1, t1) ->
      let a, r = typeKindToPoly t1 in
      returnNormalized (s1, Poly (a + 1, r))
  | Annot (_, t) -> returnNormalized (emptySubst, t)
  | List e -> (
      match e with
      | [] -> returnNormalized (emptySubst, Rho (L (1, T (Var 0))))
      | x :: xs ->
          inferType' check ctx (List xs) >>= fun (s1, t1) ->
          let t1 = getListType t1 in
          inferType' check ctx x >>= fun (s2, t2) ->
          unify [ (t1, t2) ] >>= fun s3 ->
          let s = combineSubst s3 (combineSubst s2 s1) in
          returnNormalized (s, applySubstToTypeKind s t2))

let inferType (e : Debruijn.expr) : substitution * typeKind =
  let check (e : Debruijn.expr) =
    match e with
    | TypeApp _ | FunType _ | Lam _ ->
        raise (InferException "Not supported in HM type system")
    | _ -> e
  in
  let open IntState in
  snd (runState (inferType' check emptyCtx e) ~init:0)

let inferTypeHMV e =
  let check (e : Debruijn.expr) = e in
  let open IntState in
  snd (runState (inferType' check emptyCtx e) ~init:0)

let inferTypeBD e =
  let open IntState in
  snd (runState (Bidirection.inferType Infer emptyCtx e) ~init:0)
