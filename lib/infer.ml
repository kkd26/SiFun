open DBType
open Subst
open Unify
open State

let rec inferType' (ctx : typeCtx) :
    Debruijn.expr -> (substitution * monoType) IntState.t =
  let open IntState in
  function
  | Int _ -> return (emptySubst, Int)
  | Bool _ -> return (emptySubst, Bool)
  | Unit -> return (emptySubst, Unit)
  | Var x ->
      let sigma : monoType = find x ctx in
      return (emptySubst, sigma)
  | Fun f ->
      freshName
      >>= fun x ->
      let newCtx = updateCtx ctx (Var x) in
      inferType' newCtx f
      >>= fun (s, t) -> return (s, Fun (applySubstToMonoType s (Var x), t))
  | App (f, g) ->
      inferType' ctx f
      >>= fun (s1, tf) ->
      let newCtx = applySubstToCtx s1 ctx in
      inferType' newCtx g
      >>= fun (s2, tg) ->
      freshName
      >>= fun x ->
      let tx = Fun (tg, Var x) in
      let s3 = unify [(applySubstToMonoType s2 tf, tx)] in
      return
        (combineSubst s3 (combineSubst s2 s1), applySubstToMonoType s3 (Var x))
  | Pair (e1, e2) ->
      inferType' ctx e1
      >>= fun (s1, t1) ->
      let newCtx = applySubstToCtx s1 ctx in
      inferType' newCtx e2
      >>= fun (s2, t2) ->
      let s3 = combineSubst s2 s1 in
      return (s3, Pair (applySubstToMonoType s3 t1, applySubstToMonoType s3 t2))
  | (Fst e | Snd e) as e1 ->
      inferType' ctx e
      >>= fun (s1, t1) ->
      freshName
      >>= fun x ->
      freshName
      >>= fun y ->
      let tx = Pair (Var x, Var y) in
      let s2 = unify [(tx, t1)] in
      let s3 = combineSubst s2 s1 in
      return
        ( s3
        , applySubstToMonoType s3 (match e1 with Fst _ -> Var x | _ -> Var y) )
  | _ -> failwith "not supported"

let inferType (e : Debruijn.expr) : substitution * monoType =
  let open IntState in
  snd (runState (inferType' emptyCtx e) ~init:(-1))

let inferTypeHMV e =
  let open IntState in
  let rec inferTypeHMV' (ctx : typeCtx) :
      Debruijn.expr -> (substitution * monoType) t = function
    | TypeApp (e, t) ->
        inferTypeHMV' ctx e
        >>= fun (s1, t1) ->
        freshName
        >>= fun x ->
        let tx = ForAll (Var x) in
        let s2 = unify [(tx, t1)] in
        let s3 = combineSubst s2 s1 in
        let tw = DBType.subst t 0 (applySubstToMonoType s3 (Var x)) in
        return (s3, tw)
    | FunType (t, f) ->
        let newCtx = updateCtx ctx t in
        inferType' newCtx f
        >>= fun (s1, t1) -> return (s1, Fun (applySubstToMonoType s1 t, t1))
    | Lam e -> inferTypeHMV' ctx e >>= fun (s1, t1) -> return (s1, ForAll t1)
    | t -> inferType' ctx t in
  snd (runState (inferTypeHMV' emptyCtx e) ~init:(-1))
