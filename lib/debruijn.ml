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
  | FunType of DBType.typeKind * expr
  | TypeApp of expr * DBType.typeKind
  | Lam of expr

let emptyEnvErrorMessage msg var = Printf.sprintf "%s: Unbound value %s" msg var
let emptyEnv s var = failwith (emptyEnvErrorMessage s var)
let update env x y = if y = x then 0 else 1 + env y

let toDeBruijn =
  let rec toDeBruijn' (ctx : Ast.var -> var) typeCtx : Ast.expr -> expr =
    function
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
        let tk = DBType.typeToDeBruijn typeCtx t in
        FunType (tk, toDeBruijn' newCtx typeCtx e)
    | TypeApp (e, t) ->
        let tk = DBType.typeToDeBruijn typeCtx t in
        TypeApp (toDeBruijn' ctx typeCtx e, tk)
    | Lam (v, e) ->
        let newTypeCtx = update typeCtx v in
        Lam (toDeBruijn' ctx newTypeCtx e)
  in
  toDeBruijn' (emptyEnv "Empty var env") (emptyEnv "Empty type env")

let exprToString =
  let rec exprToString' typeVar var = function
    | Int i -> string_of_int i
    | Var v -> DBType.incChar (var - v - 1)
    | Bool b -> string_of_bool b
    | Unit -> "()"
    | Pair (e1, e2) ->
        "("
        ^ exprToString' typeVar var e1
        ^ ","
        ^ exprToString' typeVar var e2
        ^ ")"
    | Fst e -> "fst " ^ exprToString' typeVar var e
    | Snd e -> "snd " ^ exprToString' typeVar var e
    | Fun e ->
        "fn " ^ DBType.incChar var ^ " => " ^ exprToString' typeVar (var + 1) e
    | FunType (t, e) ->
        "fn " ^ DBType.incChar var ^ " : " ^ DBType.typeKindToString t ^ " => "
        ^ exprToString' typeVar (var + 1) e
    | App (e1, e2) ->
        "("
        ^ exprToString' typeVar var e1
        ^ ") ("
        ^ exprToString' typeVar var e2
        ^ ")"
    | TypeApp (e, t) ->
        "("
        ^ exprToString' typeVar var e
        ^ ") {" ^ DBType.typeKindToString t ^ "}"
    | Lam e ->
        "lam " ^ DBType.incChar typeVar ^ "."
        ^ exprToString' (typeVar + 1) var e
  in
  exprToString' 1 16

let rec exprListToString = function
  | [] -> ""
  | x :: xs -> exprToString x ^ "\n" ^ exprListToString xs
