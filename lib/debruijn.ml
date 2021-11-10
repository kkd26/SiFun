type var = int

type expr =
  | Int of int
  | Var of var
  | Bool of bool
  | Unit
  | Pair of expr * expr
  | Fst of expr
  | Snd of expr
  | Fun of expr
  | App of expr * expr

let empty_env _ = failwith "Empty env"
let update env x y = if y = x then 0 else 1 + env y

let toDeBruijn =
  let rec toDeBruijn' env : Ast.expr -> expr = function
    | Int i -> Int i
    | Var v -> Var (env v)
    | Bool b -> Bool b
    | Unit -> Unit
    | Pair (e1, e2) -> Pair (toDeBruijn' env e1, toDeBruijn' env e2)
    | Fst e -> Fst (toDeBruijn' env e)
    | Snd e -> Snd (toDeBruijn' env e)
    | Fun (v, e) ->
        let newEnv = update env v in
        Fun (toDeBruijn' newEnv e)
    | App (e1, e2) -> App (toDeBruijn' env e1, toDeBruijn' env e2) in
  toDeBruijn' empty_env

let rec exprToString = function
  | Int i -> "Int(" ^ string_of_int i ^ ")"
  | Var v -> "Var(" ^ string_of_int v ^ ")"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | Unit -> "Unit"
  | Pair (e1, e2) -> "Pair(" ^ exprToString e1 ^ "," ^ exprToString e2 ^ ")"
  | Fst e -> "Fst(" ^ exprToString e ^ ")"
  | Snd e -> "Snd(" ^ exprToString e ^ ")"
  | Fun e -> "Fun(" ^ exprToString e ^ ")"
  | App (e1, e2) -> "App(" ^ exprToString e1 ^ "," ^ exprToString e2 ^ ")"

let rec exprListToString = function
  | [] -> ""
  | x :: xs -> exprToString x ^ "\n" ^ exprListToString xs