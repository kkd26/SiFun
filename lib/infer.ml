open Type
open Unify
open State

let inferType (e : Debruijn.expr) : substitution * monoType =
  let open IntState in
  let rec inferType' (ctx : typeCtx) :
      Debruijn.expr -> (substitution * monoType) t = function
    | Int _ -> return (empty_subst, Int)
    | Bool _ -> return (empty_subst, Bool)
    | Unit -> return (empty_subst, Unit)
    | Var x ->
        let sigma : monoType = find x ctx in
        return (empty_subst, sigma)
    | Fun f ->
        fresh_name
        >>= fun x ->
        let newCtx = update_ctx ctx x (Var x) in
        inferType' newCtx f
        >>= fun (s, t) -> return (s, Fun (apply_subst s (Var x), t))
    | App (f, g) -> (
        inferType' ctx f
        >>= fun (s1, tf) ->
        let newCtx = applySubstToCtx s1 ctx in
        inferType' newCtx g
        >>= fun (s2, tg) ->
        fresh_name
        >>= fun x ->
        let tx = Fun (tg, Var x) in
        try
          let s3 = unify [(apply_subst s2 tf, tx)] in
          return (comb s3 (comb s2 s1), apply_subst s3 (Var x))
        with UnifyException e -> failwith (e ^ substToString ctx) )
    | Pair (e1, e2) ->
        inferType' ctx e1
        >>= fun (s1, t1) ->
        inferType' ctx e2 >>= fun (s2, t2) -> return (comb s1 s2, Pair (t1, t2))
    | Fst e ->
        inferType' ctx e
        >>= fun (_, t1) ->
        fresh_name
        >>= fun x ->
        fresh_name
        >>= fun y ->
        let tx = Pair (Var x, Var y) in
        let s2 = unify [(tx, t1)] in
        return (s2, apply_subst s2 (Var x))
    | Snd e ->
        inferType' ctx e
        >>= fun (_, t1) ->
        fresh_name
        >>= fun x ->
        fresh_name
        >>= fun y ->
        let tx = Pair (Var x, Var y) in
        let s2 = unify [(tx, t1)] in
        return (s2, apply_subst s2 (Var y)) in
  snd (runState (inferType' empty_ctx e) ~init:0)