open DBType
open Subst
open State

exception UnifyException of string

let rec inTypeMono n t =
  match t with
  | FreshVar m ->
      if n = m then raise (UnifyException "Circular dependencies in monoType ")
      else ()
  | Pair (t1, t2) ->
      inTypeMono n t1;
      inTypeMono n t2
  | Fun (t1, t2) ->
      inTypeMono n t1;
      inTypeMono n t2
  | _ -> ()

and inTypeRho n r =
  match r with
  | RhoMono m -> inTypeMono n m
  | RhoFun (p1, p2) ->
      inTypePoly n p1;
      inTypePoly n p2
  | RhoPair (p1, p2) ->
      inTypePoly n p1;
      inTypePoly n p2
  | RhoList p -> inTypePoly n p

and inTypePoly n p =
  let _, r = p in
  inTypeRho n r

let rec unifyMono (m1 : monoType) (m2 : monoType) : substitution IntState.t =
  let open IntState in
  match (m1, m2) with
  | Int, Int | Bool, Bool | Unit, Unit -> return emptySubst
  | Var n, Var m ->
      if n = m then return emptySubst
      else raise (UnifyException "Cannot unify different type vars")
  | FreshVar n, FreshVar m ->
      if n = m then return emptySubst else return [ (n, Mono m2) ]
  | Fun (m3, m4), Fun (m5, m6) ->
      unifyList [ (Mono m3, Mono m5); (Mono m4, Mono m6) ]
  | Pair (m3, m4), Pair (m5, m6) ->
      unifyList [ (Mono m3, Mono m5); (Mono m4, Mono m6) ]
  | FreshVar _, Var _ | Var _, FreshVar _ ->
      raise (UnifyException "Cannot unify freshvar with type var")
  | FreshVar n, t | t, FreshVar n -> (
      try
        inTypeMono n t;
        return [ (n, Mono t) ]
      with UnifyException e ->
        raise
          (UnifyException
             (e ^ monoTypeToString m1 ^ " and " ^ monoTypeToString m2)))
  | _, _ ->
      raise
        (UnifyException
           ("Cannot unify " ^ monoTypeToString m1 ^ " and "
          ^ monoTypeToString m2))

and unifyRho (r1 : rhoType) (r2 : rhoType) : substitution IntState.t =
  match (r1, r2) with
  | RhoMono (Fun (a, b)), RhoFun (p1, p2) ->
      unifyList [ (Mono a, Poly p1); (Mono b, Poly p2) ]
  | RhoFun (p1, p2), RhoMono (Fun (a, b)) ->
      unifyList [ (Mono a, Poly p1); (Mono b, Poly p2) ]
  | RhoMono (Pair (a, b)), RhoPair (p1, p2) ->
      unifyList [ (Mono a, Poly p1); (Mono b, Poly p2) ]
  | RhoPair (p1, p2), RhoMono (Pair (a, b)) ->
      unifyList [ (Mono a, Poly p1); (Mono b, Poly p2) ]
  | RhoList p1, RhoList p2 -> unifyPoly p1 p2
  | RhoMono m1, RhoMono m2 -> unifyMono m1 m2
  | RhoFun (p1, p2), RhoFun (p3, p4) ->
      unifyList [ (Poly p1, Poly p3); (Poly p2, Poly p4) ]
  | RhoPair (p1, p2), RhoPair (p3, p4) ->
      unifyList [ (Poly p1, Poly p3); (Poly p2, Poly p4) ]
  | _, _ ->
      raise
        (UnifyException
           ("Cannot unify " ^ rhoToString r1 ^ " and " ^ rhoToString r2))

and unifyPoly (p1 : polyType) (p2 : polyType) : substitution IntState.t =
  let open IntState in
  let a1, r1 = p1 in
  let a2, r2 = p2 in
  if a1 = a2 then
    match a1 with
    | 0 -> unifyList [ (Rho r1, Rho r2) ]
    | n ->
        freshName >>= fun x ->
        let rx =
          typeGenreToRho (normalize (substRho (Mono (FreshVar x)) (n - 1) r1))
        in
        unifyList [ (Poly (n - 1, rx), Poly (n - 1, r2)) ]
  else
    raise
      (UnifyException
         ("Different size " ^ polyToString p1 ^ " and " ^ polyToString p2))

and unifyOne (t1 : typeGenre) (t2 : typeGenre) : substitution IntState.t =
  let open IntState in
  let t1 = normalize t1 in
  let t2 = normalize t2 in

  match (t1, t2) with
  | Mono (FreshVar x), y ->
      inTypePoly x (typeGenreToPoly y);
      return [ (x, t2) ]
  | y, Mono (FreshVar x) ->
      inTypePoly x (typeGenreToPoly y);
      return [ (x, t1) ]
  | Poly p, _ -> unifyPoly p (typeGenreToPoly t2)
  | _, Poly p -> unifyPoly (typeGenreToPoly t1) p
  | Rho r, _ -> unifyRho r (typeGenreToRho t2)
  | _, Rho r -> unifyRho (typeGenreToRho t1) r
  | Mono m, _ -> unifyMono m (typeGenreToMono t2)

and unifyList (s : (typeGenre * typeGenre) list) : substitution IntState.t =
  let open IntState in
  match s with
  | [] -> return emptySubst
  | (x, y) :: t ->
      unifyList t >>= fun s2 ->
      unifyOne (applySubstToTypeGenre s2 x) (applySubstToTypeGenre s2 y)
      >>= fun s1 -> return (s1 @ s2)
