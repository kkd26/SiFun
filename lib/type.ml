type typeVar = int

type monoType =
  | Var of typeVar
  | Int
  | Bool
  | Unit
  | Pair of monoType * monoType
  | Fun of monoType * monoType

type typeCtx = (typeVar * monoType) list

let emptyCtx : typeCtx = []
let updateCtx (ctx : typeCtx) x t : typeCtx = (x, t) :: ctx
let find (x : typeVar) (ctx : typeCtx) : monoType = snd (List.nth ctx x)

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
  let range = List.init n (fun x -> x) in
  List.combine range list

exception TypeException
exception UnifyException of string

let numToString n =
  let i = n / 26 in
  String.make 1 (Char.chr (Char.code 'a' + (n mod 26)))
  ^ if i <> 0 then string_of_int i else ""

let rec typeExprToString = function
  | Var n -> numToString n
  | Int -> "int"
  | Bool -> "bool"
  | Unit -> "unit"
  | Pair (t1, t2) ->
      "(" ^ typeExprToString t1 ^ ", " ^ typeExprToString t2 ^ ")"
  | Fun (t1, t2) ->
      let inner = typeExprToString t1 in
      (match t1 with Fun (_, _) -> "(" ^ inner ^ ")" | _ -> inner)
      ^ " -> " ^ typeExprToString t2

let substToString =
  let elemToString a (x, t) =
    a ^ string_of_int x ^ ":(" ^ typeExprToString t ^ ") " in
  List.fold_left elemToString ""
