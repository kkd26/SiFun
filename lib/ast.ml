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

let rec exprToString = function
  | Int i -> "Int(" ^ string_of_int i ^ ")"
  | Var v -> "Var(" ^ v ^ ")"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | Unit -> "Unit"
  | Pair (e1, e2) -> "Pair(" ^ exprToString e1 ^ "," ^ exprToString e2 ^ ")"
  | Fst e -> "Fst(" ^ exprToString e ^ ")"
  | Snd e -> "Snd(" ^ exprToString e ^ ")"
  | Fun (v, e) -> "Fun(" ^ v ^ "," ^ exprToString e ^ ")"
  | App (e1, e2) -> "App(" ^ exprToString e1 ^ "," ^ exprToString e2 ^ ")"

let rec exprListToString = function
  | [] -> ""
  | x :: xs -> exprToString x ^ "\n" ^ exprListToString xs
