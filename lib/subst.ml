open Type

type typeCtx = (typeVar * monoType) list

let emptyCtx : typeCtx = []
let updateCtx (ctx : typeCtx) x t : typeCtx = (x, t) :: ctx
let find (x : int) (ctx : typeCtx) : monoType = snd (List.nth ctx x)

type substitution = (typeVar * monoType) list

let emptySubst : substitution = []

let rec subst (s : monoType) (x : typeVar) (t : monoType) : monoType =
  match t with
  | Var y -> if x = y then s else t
  | Fun (t1, t2) -> Fun (subst s x t1, subst s x t2)
  | Pair (t1, t2) -> Pair (subst s x t1, subst s x t2)
  | t -> t

let applySubstToMonoType (s : substitution) (t : monoType) : monoType =
  List.fold_right (fun (x, s) -> subst s x) s t

let applySubstToCtx s (ctx : typeCtx) : typeCtx =
  List.map (fun (x, t) -> (x, applySubstToMonoType s t)) ctx

let combineSubst s1 s2 : substitution =
  List.map (fun (x, t) -> (x, applySubstToMonoType s1 t)) (s1 @ s2)

let substFromList list : substitution =
  let n = List.length list in
  let range = List.init n (fun x -> numToString x) in
  List.combine range list

exception TypeException
exception UnifyException of string

let substToString =
  let elemToString a (x, t) = a ^ x ^ ":(" ^ typeExprToString t ^ ") " in
  List.fold_left elemToString ""