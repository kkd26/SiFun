open DBType
open Subst
open Unify
open State
open TypeCtx

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
      let tx = Mono (Fun (typeKindToMono tg, FreshVar x)) in
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
      returnNormalized (s3, Mono (Pair (typeKindToMono m1, typeKindToMono m2)))
  | (Fst e | Snd e) as e1 ->
      inferType' check ctx e >>= fun (s1, t1) ->
      freshName >>= fun x ->
      freshName >>= fun y ->
      let tx = Mono (Pair (FreshVar x, FreshVar y)) in
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
        DBType.substType (typeKindToMono t) 0
          (applySubstToTypeKind s3 (Mono (FreshVar x)))
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

let inferType (e : Debruijn.expr) : substitution * typeKind =
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
  try snd (runState (inferType' check emptyCtx e) ~init:0) with
  | UnifyException e -> failwith e
  | TypeException -> failwith "incorrect type"

let inst (s : polyType) =
  let boundVariables, r = s in
  let rec inst' r n =
    let open IntState in
    freshName >>= fun x ->
    match n with
    | 0 -> return r
    | n -> inst' (DBType.substRho (FreshVar x) (n - 1) r) (n - 1)
  in
  inst' r boundVariables

(* let rec dsk (s2 : polyType) : polyType IntState.t =
     let open IntState in
     pr s2 >>= fun (a, r) ->

   and dsk' ((a,r) : polyType) : rho IntState.t =
     match r with
     |T t -> return (T t)
     |F(s1,s2) -> dsk' s2 >>= fun r4 -> *)

(* let rec infer (ctx : TypeCtx.typeCtx) (e : Debruijn.expr) =
     let open IntState in
     match e with
     | Int _ -> return (T Int)
     | Var v ->
         let s = TypeCtx.find v ctx in
         inst s
     | Fun e ->
         freshName >>= fun x ->
         let s = (0, T (FreshVar x)) in
         let newCtx = TypeCtx.updateCtx ctx s in
         infer newCtx e >>= fun r -> return (F (s, (0, r)))
     | App (t, u) -> (
         infer ctx t >>= fun s ->
         match s with
         | F (s1, s2) -> gen ctx u >>= fun s' -> dsk s' >>= fun s1' -> inst s2
         | T _ -> failwith "error")
     | _ -> return (T Int)

   and gen (ctx : TypeCtx.typeCtx) (t : Debruijn.expr) =
     let open IntState in
     infer ctx t >>= fun r ->
     let a = DBType.freeVarRho r - TypeCtx.freeVarCtx ctx in
     return (a, r) *)
