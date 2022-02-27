type var = string

type expr =
  | Int of int
  | Var of var
  | Bool of bool
  | Unit
  | Pair of expr * expr
  | Fst of expr
  | Snd of expr
  | Fun of var * expr
  | App of expr * expr
  (* Extended Language *)
  | FunType of var * Type.monoType * expr
  | TypeApp of expr * Type.monoType
  | Lam of Type.typeVar * expr
  | Annot of expr * Type.monoType

let bigLamFromList typeVarList expr =
  List.fold_right (fun v e -> Lam (v, e)) typeVarList expr

let rec exprToString = function
  | Int i -> "Int(" ^ string_of_int i ^ ")"
  | Var v -> "Var(" ^ v ^ ")"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | Unit -> "Unit"
  | Pair (e1, e2) -> "Pair(" ^ exprToString e1 ^ "," ^ exprToString e2 ^ ")"
  | Fst e -> "Fst(" ^ exprToString e ^ ")"
  | Snd e -> "Snd(" ^ exprToString e ^ ")"
  | Fun (v, e) -> "Fun(" ^ v ^ "," ^ exprToString e ^ ")"
  | FunType (v, t, e) ->
      "Fun(" ^ v ^ "," ^ Type.typeExprToString t ^ "," ^ exprToString e ^ ")"
  | App (e1, e2) -> "App(" ^ exprToString e1 ^ "," ^ exprToString e2 ^ ")"
  | TypeApp (e, t) ->
      "TypeApp(" ^ exprToString e ^ "," ^ Type.typeExprToString t ^ ")"
  | Lam (v, e) -> "Lam(" ^ v ^ "," ^ exprToString e ^ ")"
  | Annot (e, t) ->
      "Annot(" ^ exprToString e ^ "," ^ Type.typeExprToString t ^ ")"

let rec exprListToString = function
  | [] -> ""
  | x :: xs -> exprToString x ^ "\n" ^ exprListToString xs
