type var = string

type expr =
  | Int of int
  | Var of var
  | Bool of bool
  | Unit
  | Pair of expr * expr
  | Fun of var * expr
  | App of expr * expr
