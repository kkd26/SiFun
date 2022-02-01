open DBType
open Subst
open State

let rec inType n t =
  match t with
  | FreshVar m ->
      if n = m then raise (UnifyException "Circular dependencies ") else true
  | Pair (t1, t2) -> inType n t1 && inType n t2
  | Fun (t1, t2) -> inType n t1 && inType n t2
  | ForAll t1 -> inType n t1
  | _ -> true

let rec unifyOne (t1 : monoType) (t2 : monoType) : substitution IntState.t =
  let open IntState in
  match (t1, t2) with
  | Int, Int | Bool, Bool | Unit, Unit -> return emptySubst
  | Var n, Var m ->
      if n = m then return emptySubst else raise (UnifyException "")
  | FreshVar n, FreshVar m ->
      if n = m then return emptySubst else return [ (n, t2) ]
  | Fun (t3, t4), Fun (t5, t6) -> unify [ (t5, t3); (t4, t6) ]
  | Pair (t3, t4), Pair (t5, t6) -> unify [ (t3, t5); (t4, t6) ]
  | ForAll t1, ForAll t2 ->
      freshName >>= fun x -> unifyOne (DBType.subst (FreshVar x) 0 t1) t2
  | FreshVar n, t | t, FreshVar n -> (
      try
        let _ = inType n t in
        return [ (n, t) ]
      with UnifyException e ->
        raise
          (UnifyException
             (e ^ typeExprToString t1 ^ " and " ^ typeExprToString t2)))
  | _, _ ->
      raise
        (UnifyException
           ("Cannot unify " ^ typeExprToString t1 ^ " and "
          ^ typeExprToString t2))

and unify s : substitution IntState.t =
  let open IntState in
  match s with
  | [] -> return emptySubst
  | (x, y) :: t ->
      unify t >>= fun s2 ->
      unifyOne (applySubstToMonoType s2 x) (applySubstToMonoType s2 y)
      >>= fun s1 -> return (s1 @ s2)
