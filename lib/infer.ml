open Type
open Unify
open State

let inferType (e : Debruijn.expr) : substitution * monoType =
  let open IntState in
  let rec inferType' (ctx : typeCtx) :
      Debruijn.expr -> (substitution * monoType) t = function
    | Int _ -> return (emptySubst, Int)
    | Bool _ -> return (emptySubst, Bool)
    | Unit -> return (emptySubst, Unit)
    | Var x ->
        let sigma : monoType = find x ctx in
        return (emptySubst, sigma)
    | Fun f ->
        freshName
        >>= fun x ->
        let newCtx = updateCtx ctx x (Var x) in
        inferType' newCtx f
        >>= fun (s, t) -> return (s, Fun (applySubstToMonoType s (Var x), t))
    | App (f, g) -> (
        inferType' ctx f
        >>= fun (s1, tf) ->
        let newCtx = applySubstToCtx s1 ctx in
        inferType' newCtx g
        >>= fun (s2, tg) ->
        freshName
        >>= fun x ->
        let tx = Fun (tg, Var x) in
        try
          let s3 = unify [(applySubstToMonoType s2 tf, tx)] in
          return
            ( combineSubst s3 (combineSubst s2 s1)
            , applySubstToMonoType s3 (Var x) )
        with UnifyException e -> failwith (e ^ substToString ctx) )
    | Pair (e1, e2) ->
        inferType' ctx e1
        >>= fun (s1, t1) ->
        let newCtx = applySubstToCtx s1 ctx in
        inferType' newCtx e2
        >>= fun (s2, t2) ->
        let s3 = combineSubst s2 s1 in
        return
          (s3, Pair (applySubstToMonoType s3 t1, applySubstToMonoType s3 t2))
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
          , applySubstToMonoType s3 (match e1 with Fst _ -> Var x | _ -> Var y)
          ) in
  snd (runState (inferType' emptyCtx e) ~init:0)