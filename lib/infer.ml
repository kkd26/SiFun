open DBType
open Subst
open Unify
open State

let rec inferType' check (ctx : typeCtx) (e : Debruijn.expr) :
    (substitution * monoType) IntState.t =
  let _ = check e in
  let open IntState in
  match e with
  | Int _ -> return (emptySubst, Int)
  | Bool _ -> return (emptySubst, Bool)
  | Unit -> return (emptySubst, Unit)
  | Var x ->
      let sigma : monoType = find x ctx in
      return (emptySubst, sigma)
  | Fun f ->
      freshName >>= fun x ->
      let newCtx = updateCtx ctx (FreshVar x) in
      inferType' check newCtx f >>= fun (s, t) ->
      return (s, Fun (applySubstToMonoType s (FreshVar x), t))
  | App (f, g) ->
      inferType' check ctx f >>= fun (s1, tf) ->
      let newCtx = applySubstToCtx s1 ctx in
      inferType' check newCtx g >>= fun (s2, tg) ->
      freshName >>= fun x ->
      let tx = Fun (tg, FreshVar x) in
      let s3 = unify [ (applySubstToMonoType s2 tf, tx) ] in
      return
        ( combineSubst s3 (combineSubst s2 s1),
          applySubstToMonoType s3 (FreshVar x) )
  | Pair (e1, e2) ->
      inferType' check ctx e1 >>= fun (s1, t1) ->
      let newCtx = applySubstToCtx s1 ctx in
      inferType' check newCtx e2 >>= fun (s2, t2) ->
      let s3 = combineSubst s2 s1 in
      return (s3, Pair (applySubstToMonoType s3 t1, applySubstToMonoType s3 t2))
  | (Fst e | Snd e) as e1 ->
      inferType' check ctx e >>= fun (s1, t1) ->
      freshName >>= fun x ->
      freshName >>= fun y ->
      let tx = Pair (FreshVar x, FreshVar y) in
      let s2 = unify [ (tx, t1) ] in
      let s3 = combineSubst s2 s1 in
      return
        ( s3,
          applySubstToMonoType s3
            (match e1 with Fst _ -> FreshVar x | _ -> FreshVar y) )
  | TypeApp (e, t) ->
      inferType' check ctx e >>= fun (s1, t1) ->
      freshName >>= fun x ->
      let tx = ForAll (FreshVar x) in
      let s2 = unify [ (tx, t1) ] in
      let s3 = combineSubst s2 s1 in
      let tw = DBType.subst t 0 (applySubstToMonoType s3 (FreshVar x)) in
      return (s3, tw)
  | FunType (t, f) ->
      let newCtx = updateCtx ctx t in
      inferType' check newCtx f >>= fun (s1, t1) ->
      return (s1, Fun (applySubstToMonoType s1 t, t1))
  | Lam e -> inferType' check ctx e >>= fun (s1, t1) -> return (s1, ForAll t1)

let inferType (e : Debruijn.expr) : substitution * monoType =
  let check (e : Debruijn.expr) =
    match e with
    | TypeApp _ | FunType _ | Lam _ -> failwith "Not supported"
    | _ -> e
  in
  let open IntState in
  try snd (runState (inferType' check emptyCtx e) ~init:0)
  with UnifyException e -> failwith e

let inferTypeHMV e =
  let check (e : Debruijn.expr) = e in
  let open IntState in
  try snd (runState (inferType' check emptyCtx e) ~init:0)
  with UnifyException e -> failwith e
