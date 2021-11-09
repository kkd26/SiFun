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

let rec printExp = function
  | Int i -> "Int(" ^ string_of_int i ^ ")"
  | Var v -> "Var(" ^ v ^ ")"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | Unit -> "Unit"
  | Pair (e1, e2) -> "Pair(" ^ printExp e1 ^ "," ^ printExp e2 ^ ")"
  | Fst e -> "Fst(" ^ printExp e ^ ")"
  | Snd e -> "Snd(" ^ printExp e ^ ")"
  | Fun (v, e) -> "Fun(" ^ v ^ "," ^ printExp e ^ ")"
  | App (e1, e2) -> "App(" ^ printExp e1 ^ "," ^ printExp e2 ^ ")"

let rec printExpList = function
  | [] -> ""
  | x :: xs -> printExp x ^ "\n" ^ printExpList xs
