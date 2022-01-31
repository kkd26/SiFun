open DBType

type typeCtx = monoType list

let emptyCtx : typeCtx = []
let updateCtx (ctx : typeCtx) (t : monoType) = t :: ctx
let find (x : int) (ctx : typeCtx) : monoType = List.nth ctx x
let shift i c = List.map (shift i c)

type substitution = (typeVar * monoType) list

let emptySubst : substitution = []

let rec subst (s : monoType) (x : typeVar) (t : monoType) : monoType =
  match t with
  | FreshVar y -> if x = y then s else t
  | Fun (t1, t2) -> Fun (subst s x t1, subst s x t2)
  | Pair (t1, t2) -> Pair (subst s x t1, subst s x t2)
  | t -> t

let applySubstToMonoType (s : substitution) (t : monoType) : monoType =
  List.fold_right (fun (x, s) -> subst s x) s t

let applySubstToCtx s (ctx : typeCtx) : typeCtx =
  List.map (applySubstToMonoType s) ctx

let combineSubst s1 s2 : substitution =
  List.map (fun (x, t) -> (x, applySubstToMonoType s1 t)) (s1 @ s2)

let substFromList list : substitution =
  let n = List.length list in
  let range = List.init n (fun x -> x) in
  List.combine range list

exception TypeException
exception UnifyException of string

let substToString =
  let elemToString a (x, t) =
    a ^ string_of_int x ^ ":(" ^ typeExprToString t ^ ") "
  in
  List.fold_left elemToString "e "
