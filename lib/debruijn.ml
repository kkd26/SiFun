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
  (* Extended Language *)
  | FunType of DBType.monoType * expr
  | TypeApp of expr * DBType.monoType
  | Lam of expr

let emptyEnvErrorMessage msg var = Printf.sprintf "%s: Unbound value %s" msg var
let emptyEnv s var = failwith (emptyEnvErrorMessage s var)
let update env x y = if y = x then 0 else 1 + env y

let toDeBruijn =
  let rec toDeBruijn' ctx typeCtx : Ast.expr -> expr = function
    | Int i -> Int i
    | Var v -> Var (ctx v)
    | Bool b -> Bool b
    | Unit -> Unit
    | Pair (e1, e2) ->
        Pair (toDeBruijn' ctx typeCtx e1, toDeBruijn' ctx typeCtx e2)
    | Fst e -> Fst (toDeBruijn' ctx typeCtx e)
    | Snd e -> Snd (toDeBruijn' ctx typeCtx e)
    | Fun (v, e) ->
        let newCtx = update ctx v in
        Fun (toDeBruijn' newCtx typeCtx e)
    | App (e1, e2) ->
        App (toDeBruijn' ctx typeCtx e1, toDeBruijn' ctx typeCtx e2)
    | FunType (v, t, e) ->
        let newCtx = update ctx v in
        FunType (DBType.toDeBruijn typeCtx t, toDeBruijn' newCtx typeCtx e)
    | TypeApp (e, t) ->
        TypeApp (toDeBruijn' ctx typeCtx e, DBType.toDeBruijn typeCtx t)
    | Lam (v, e) ->
        let newTypeCtx = update typeCtx v in
        Lam (toDeBruijn' ctx newTypeCtx e)
  in
  toDeBruijn' (emptyEnv "Empty var env") (emptyEnv "Empty type env")

let rec exprToString = function
  | Int i -> "Int(" ^ string_of_int i ^ ")"
  | Var v -> "Var(" ^ string_of_int v ^ ")"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | Unit -> "Unit"
  | Pair (e1, e2) -> "Pair(" ^ exprToString e1 ^ "," ^ exprToString e2 ^ ")"
  | Fst e -> "Fst(" ^ exprToString e ^ ")"
  | Snd e -> "Snd(" ^ exprToString e ^ ")"
  | Fun e -> "Fun(" ^ exprToString e ^ ")"
  | FunType (t, e) ->
      "FunType(" ^ DBType.typeExprToString t ^ "," ^ exprToString e ^ ")"
  | App (e1, e2) -> "App(" ^ exprToString e1 ^ "," ^ exprToString e2 ^ ")"
  | TypeApp (e, t) ->
      "TypeApp(" ^ exprToString e ^ ", " ^ DBType.typeExprToString t ^ ")"
  | Lam e -> "L " ^ "." ^ exprToString e

let rec exprListToString = function
  | [] -> ""
  | x :: xs -> exprToString x ^ "\n" ^ exprListToString xs
