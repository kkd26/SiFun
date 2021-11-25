open Type
open Subst

let rec inType n t =
  match t with
  | Var m ->
      if n = m then raise (UnifyException "Circular dependencies ") else true
  | Pair (t1, t2) -> inType n t1 && inType n t2
  | Fun (t1, t2) -> inType n t1 && inType n t2
  | _ -> true

let rec unifyOne (t1 : monoType) (t2 : monoType) : substitution =
  match (t1, t2) with
  | Int, Int | Bool, Bool | Unit, Unit -> emptySubst
  | Var n, Var m -> if n = m then emptySubst else [(n, t2)]
  | Fun (t3, t4), Fun (t5, t6) -> unify [(t3, t5); (t4, t6)]
  | Pair (t3, t4), Pair (t5, t6) -> unify [(t3, t5); (t4, t6)]
  | Var n, t | t, Var n -> (
    try
      let _ = inType n t in
      [(n, t)]
    with UnifyException e ->
      raise
        (UnifyException (e ^ typeExprToString t1 ^ " and " ^ typeExprToString t2)
        ) )
  | _, _ ->
      raise
        (UnifyException
           ( "Cannot unify " ^ typeExprToString t1 ^ " and "
           ^ typeExprToString t2 ) )

and unify s : substitution =
  match s with
  | [] -> []
  | (x, y) :: t ->
      let t2 = unify t in
      let t1 =
        unifyOne (applySubstToMonoType t2 x) (applySubstToMonoType t2 y) in
      t1 @ t2
