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
  | T m -> inTypeMono n m
  | F (p1, p2) ->
      inTypePoly n p1;
      inTypePoly n p2
  | P (p1, p2) ->
      inTypePoly n p1;
      inTypePoly n p2

and inTypePoly n p =
  let _, r = p in
  inTypeRho n r

let rec unifyMono (m1 : monoType) (m2 : monoType) : substitution IntState.t =
  let open IntState in
  match (m1, m2) with
  | Int, Int | Bool, Bool | Unit, Unit -> return emptySubst
  | Var n, Var m ->
      if n = m then return emptySubst else raise (UnifyException "")
  | FreshVar n, FreshVar m ->
      if n = m then return emptySubst else return [ (n, Mono m2) ]
  | Fun (m3, m4), Fun (m5, m6) ->
      unify [ (Mono m3, Mono m5); (Mono m4, Mono m6) ]
  | Pair (m3, m4), Pair (m5, m6) ->
      unify [ (Mono m3, Mono m5); (Mono m4, Mono m6) ]
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
  | T m1, T m2 -> unifyMono m1 m2
  | F (p1, p2), F (p3, p4) -> unify [ (Poly p3, Poly p1); (Poly p2, Poly p4) ]
  | P (p1, p2), P (p3, p4) -> unify [ (Poly p1, Poly p3); (Poly p2, Poly p4) ]
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
    | 0 -> unify [ (Rho r1, Rho r2) ]
    | n ->
        freshName >>= fun x ->
        unify
          [
            ( Poly
                ( n - 1,
                  DBType.typeKindToRho
                    (DBType.substRho (Mono (FreshVar x)) 0 r1) ),
              Poly (n - 1, r2) );
          ]
  else
    raise
      (UnifyException
         ("Different size " ^ polyToString p1 ^ " and " ^ polyToString p2))

and unifyOne (t1 : typeKind) (t2 : typeKind) : substitution IntState.t =
  let t2 =
    match t1 with
    | Mono _ -> Mono (typeKindToMono t2)
    | Rho _ -> Rho (typeKindToRho t2)
    | Poly _ -> Poly (typeKindToPoly t2)
  in
  match (t1, t2) with
  | Mono m1, Mono m2 -> unifyMono m1 m2
  | Rho r1, Rho r2 -> unifyRho r1 r2
  | Poly p1, Poly p2 -> unifyPoly p1 p2
  | _, _ ->
      raise
        (UnifyException
           ("Cannot unify " ^ typeKindToString t1 ^ " and "
          ^ typeKindToString t2))

and unify (s : (typeKind * typeKind) list) : substitution IntState.t =
  let open IntState in
  match s with
  | [] -> return emptySubst
  | (x, y) :: t ->
      unify t >>= fun s2 ->
      unifyOne (applySubstToTypeKind s2 x) (applySubstToTypeKind s2 y)
      >>= fun s1 -> return (s1 @ s2)
