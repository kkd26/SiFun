open DBType
open State
open Unify
open Subst
open TermCtx
open Prenex
open Deepskolem
open Direction

let returnCombinedSubst s (sub, typ) =
  let open IntState in
  return (combineSubst sub s, normalize typ)

let rec inferType' (ctx : TermCtx.termCtx) (e : DBAst.expr) =
  let open IntState in
  match e with
  | Int _ -> returnNormalized (emptySubst, Mono Int)
  | Bool _ -> returnNormalized (emptySubst, Mono Bool)
  | Unit -> returnNormalized (emptySubst, Mono Unit)
  | Var x ->
      let tk : typeGenre = find x ctx in
      inst Infer tk
  | Fun f ->
      freshName >>= fun x ->
      let tx = Mono (FreshVar x) in
      let newCtx = updateCtx ctx tx in
      inferType' newCtx f >>= fun (s, t) ->
      let tk = applySubstToTypeGenre s tx in
      returnNormalized (s, Rho (RhoFun (typeGenreToPoly tk, typeGenreToPoly t)))
  | App (f, g) ->
      inferType' ctx f >>= fun (s1, tf) ->
      let newCtx = applySubstToCtx s1 ctx in
      freshName >>= fun x ->
      freshName >>= fun y ->
      unifyList [ (Mono (Fun (FreshVar x, FreshVar y)), tf) ] >>= fun s2 ->
      let tx = applySubstToTypeGenre s2 (Mono (FreshVar x)) in
      let newCtx = applySubstToCtx s2 newCtx in
      gen (Check tx) newCtx g >>= fun (s3, _) ->
      let s = combineSubst s3 (combineSubst s2 s1) in
      inst Infer (applySubstToTypeGenre s (Mono (FreshVar y)))
      >>= returnCombinedSubst s
  | Pair (e1, e2) ->
      inferType' ctx e1 >>= fun (s1, t1) ->
      let newCtx = applySubstToCtx s1 ctx in
      (* you have to leave it *)
      inferType' newCtx e2 >>= fun (s2, t2) ->
      let s3 = combineSubstUnique s1 s2 in
      returnNormalized
        (s3, Rho (RhoPair (typeGenreToPoly t1, typeGenreToPoly t2)))
  | (Fst e | Snd e) as e1 ->
      inferType' ctx e >>= fun (s1, t1) ->
      freshName >>= fun x ->
      freshName >>= fun y ->
      let tx =
        normalize
          (Rho
             (RhoPair
                ( typeGenreToPoly (Mono (FreshVar x)),
                  typeGenreToPoly (Mono (FreshVar y)) )))
      in
      unifyList [ (tx, t1) ] >>= fun s2 ->
      let s3 = combineSubst s2 s1 in
      returnNormalized
        ( s3,
          applySubstToTypeGenre s3
            (match e1 with
            | Fst _ -> Mono (FreshVar x)
            | _ -> Mono (FreshVar y)) )
  | TypeApp (e, t) ->
      inferType' ctx e >>= fun (s1, t1) ->
      freshName >>= fun x ->
      let tx = Mono (FreshVar x) in
      unifyList [ (tx, t1) ] >>= fun s2 ->
      let s3 = combineSubst s2 s1 in
      let tw =
        DBType.substType t 0 (applySubstToTypeGenre s3 (Mono (FreshVar x)))
      in
      returnNormalized (s3, tw)
  | FunType (t, f) ->
      let newCtx = updateCtx ctx t in
      inferType' newCtx f >>= fun (s1, t1) ->
      let m1 = applySubstToTypeGenre s1 t in
      returnNormalized
        (s1, Rho (RhoFun (typeGenreToPoly m1, typeGenreToPoly t1)))
  | Lam e ->
      inferType' (shift 1 0 ctx) e >>= fun (s1, t1) ->
      let a, r = typeGenreToPoly t1 in
      returnNormalized (s1, Poly (a + 1, r))
  | Annot (e, p) -> gen (Check p) ctx e >>= fun (_, _) -> inst Infer p
  | List e -> (
      match e with
      | [] ->
          freshName >>= fun x ->
          returnNormalized (emptySubst, Mono (List (FreshVar x)))
      | x :: xs ->
          inferType' ctx (List xs) >>= fun (s1, t1) ->
          let t1 = getListType t1 in
          inferType' ctx x >>= fun (s2, t2) ->
          unifyList [ (t1, t2) ] >>= fun s3 ->
          let s = combineSubst s3 (combineSubst s2 s1) in
          let t = typeGenreToList (applySubstToTypeGenre s t1) in
          returnNormalized (s, t))
  | Head e ->
      inferType' ctx e >>= fun (s1, t1) ->
      freshName >>= fun x ->
      let tx =
        normalize (Rho (RhoList (typeGenreToPoly (Mono (FreshVar x)))))
      in
      unifyList [ (tx, t1) ] >>= fun s2 ->
      let s3 = combineSubst s2 s1 in
      returnNormalized (s3, applySubstToTypeGenre s3 (Mono (FreshVar x)))
  | Tail e ->
      inferType' ctx e >>= fun (s1, t1) ->
      freshName >>= fun x ->
      let tx =
        normalize (Rho (RhoList (typeGenreToPoly (Mono (FreshVar x)))))
      in
      unifyList [ (tx, t1) ] >>= fun s2 ->
      let s3 = combineSubst s2 s1 in
      returnNormalized (s3, applySubstToTypeGenre s3 (Mono (List (FreshVar x))))

and check' (tk : typeGenre) (ctx : TermCtx.termCtx) (e : DBAst.expr) =
  let open IntState in
  match e with
  | Int _ ->
      unifyList [ (tk, Mono Int) ] >>= fun s -> returnNormalized (s, Mono Int)
  | Bool _ ->
      unifyList [ (tk, Mono Bool) ] >>= fun s -> returnNormalized (s, Mono Bool)
  | Unit ->
      unifyList [ (tk, Mono Unit) ] >>= fun s -> returnNormalized (s, Mono Unit)
  | Var x ->
      let tk1 : typeGenre = find x ctx in
      inst (Check tk) tk1
  | Fun f ->
      freshName >>= fun x ->
      let newCtx = updateCtx ctx (Mono (FreshVar x)) in
      freshName >>= fun y ->
      let tx = Mono (Fun (FreshVar x, FreshVar y)) in
      unifyList [ (tx, tk) ] >>= fun s ->
      let newCtx = applySubstToCtx s newCtx in
      let newDir = Check (applySubstToTypeGenre s (Mono (FreshVar y))) in
      gen newDir newCtx f >>= fun (s1, t) ->
      let s = combineSubst s1 s in
      let tk = applySubstToTypeGenre s (Mono (FreshVar x)) in
      returnNormalized (s, Rho (RhoFun (typeGenreToPoly tk, typeGenreToPoly t)))
  | App (f, g) ->
      inferType' ctx f >>= fun (s1, tf) ->
      let newCtx = applySubstToCtx s1 ctx in
      freshName >>= fun x ->
      freshName >>= fun y ->
      unifyList [ (Mono (Fun (FreshVar x, FreshVar y)), tf) ] >>= fun s2 ->
      let tx = applySubstToTypeGenre s2 (Mono (FreshVar x)) in
      gen (Check tx) newCtx g >>= fun (s3, _) ->
      let s = combineSubst s3 (combineSubst s2 s1) in
      inst (Check tk) (applySubstToTypeGenre s (Mono (FreshVar y)))
      >>= returnCombinedSubst s
  | Pair (e1, e2) ->
      freshName >>= fun x ->
      freshName >>= fun y ->
      unifyList [ (Mono (Pair (FreshVar x, FreshVar y)), tk) ] >>= fun s ->
      let tx = applySubstToTypeGenre s (Mono (FreshVar x)) in
      check' tx ctx e1 >>= fun (s1, t1) ->
      let newCtx = applySubstToCtx s1 ctx in
      let ty = applySubstToTypeGenre s (Mono (FreshVar y)) in
      check' ty newCtx e2 >>= fun (s2, t2) ->
      let s3 = combineSubst s1 s2 in
      returnNormalized
        (s3, Rho (RhoPair (typeGenreToPoly t1, typeGenreToPoly t2)))
  | (Fst _ | Snd _) as e1 ->
      freshName >>= fun x ->
      freshName >>= fun y ->
      let tx =
        normalize
          (Rho
             (RhoPair
                ( typeGenreToPoly (Mono (FreshVar x)),
                  typeGenreToPoly (Mono (FreshVar y)) )))
      in
      unifyList [ (tx, tk) ] >>= fun s ->
      returnNormalized
        ( s,
          applySubstToTypeGenre s
            (match e1 with
            | Fst _ -> Mono (FreshVar x)
            | _ -> Mono (FreshVar y)) )
  | TypeApp (e, t) ->
      check' tk ctx e >>= fun (s1, t1) ->
      freshName >>= fun x ->
      let tx = Mono (FreshVar x) in
      unifyList [ (tx, t1) ] >>= fun s2 ->
      let s3 = combineSubst s2 s1 in
      let tw =
        DBType.substType t 0 (applySubstToTypeGenre s3 (Mono (FreshVar x)))
      in
      returnNormalized (s3, tw)
  | FunType (t, f) ->
      freshName >>= fun x ->
      freshName >>= fun y ->
      unifyList [ (Mono (Fun (FreshVar x, FreshVar y)), tk) ] >>= fun s ->
      let tx = applySubstToTypeGenre s (Mono (FreshVar x)) in
      inst (Check t) tx >>= fun (s1, t1) ->
      let newCtx = updateCtx ctx t1 in
      let s = combineSubst s1 s in
      let tk = applySubstToTypeGenre s (Mono (FreshVar y)) in
      check' tk newCtx f >>= fun (s1, t1) ->
      let m1 = applySubstToTypeGenre s1 t1 in
      returnNormalized
        (s1, Rho (RhoFun (typeGenreToPoly m1, typeGenreToPoly t1)))
  | Lam e ->
      check' tk (shift 1 0 ctx) e >>= fun (s1, t1) ->
      let a, r = typeGenreToPoly t1 in
      returnNormalized (s1, Poly (a + 1, r))
  | Annot (e, t) ->
      gen (Check t) ctx e >>= fun (_, _) ->
      freshName >>= fun x ->
      let tx = Mono (FreshVar x) in
      inst (Check tx) t
  | List e ->
      inferType' ctx (List e) >>= fun (s1, t1) ->
      unifyList [ (tk, t1) ] >>= fun s2 ->
      let s = combineSubst s2 s1 in
      returnNormalized (s, applySubstToTypeGenre s tk)
  | Head _ ->
      freshName >>= fun x ->
      let tx =
        normalize (Rho (RhoList (typeGenreToPoly (Mono (FreshVar x)))))
      in
      unifyList [ (tx, tk) ] >>= fun s ->
      returnNormalized (s, applySubstToTypeGenre s (Mono (FreshVar x)))
  | Tail e ->
      check' tk ctx e >>= fun (s1, t1) ->
      freshName >>= fun x ->
      let tx =
        normalize (Rho (RhoList (typeGenreToPoly (Mono (FreshVar x)))))
      in
      unifyList [ (tx, t1) ] >>= fun s2 ->
      let s3 = combineSubst s2 s1 in
      returnNormalized (s3, applySubstToTypeGenre s3 (Mono (List (FreshVar x))))

and gen = function
  | Infer -> failwith "not supported"
  | Check tk ->
      let _, r = pr tk in
      check' (normalize (Rho r))

let inferType = function Infer -> inferType' | Check tx -> check' tx
