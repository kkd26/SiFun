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
  | Annot of expr * DBType.typeKind
  | List of expr list

exception DBAstException of string

let emptyEnvErrorMessage msg var =
  Printf.sprintf "%s - unbound value %s" msg var

let emptyEnv s var = raise (DBAstException (emptyEnvErrorMessage s var))

let update env x y = if y = x then 0 else 1 + env y

let toDeBruijn =
  let rec toDeBruijn' (ctx : Ast.var -> var) termCtx : Ast.expr -> expr =
    function
    | Int i -> Int i
    | Var v -> Var (ctx v)
    | Bool b -> Bool b
    | Unit -> Unit
    | Pair (e1, e2) ->
        Pair (toDeBruijn' ctx termCtx e1, toDeBruijn' ctx termCtx e2)
    | Fst e -> Fst (toDeBruijn' ctx termCtx e)
    | Snd e -> Snd (toDeBruijn' ctx termCtx e)
    | Fun (v, e) ->
        let newCtx = update ctx v in
        Fun (toDeBruijn' newCtx termCtx e)
    | App (e1, e2) ->
        App (toDeBruijn' ctx termCtx e1, toDeBruijn' ctx termCtx e2)
    | FunType (v, t, e) ->
        let newCtx = update ctx v in
        let tk = DBType.typeToDeBruijn termCtx t in
        FunType (tk, toDeBruijn' newCtx termCtx e)
    | TypeApp (e, t) ->
        let tk = DBType.typeToDeBruijn termCtx t in
        TypeApp (toDeBruijn' ctx termCtx e, tk)
    | Lam (v, e) ->
        let newTermCtx = update termCtx v in
        Lam (toDeBruijn' ctx newTermCtx e)
    | Annot (e, t) ->
        let tk = DBType.typeToDeBruijn termCtx t in
        Annot (toDeBruijn' ctx termCtx e, tk)
    | List e -> List (List.map (toDeBruijn' ctx termCtx) e)
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
    | Annot (e, t) ->
        "("
        ^ exprToString' typeVar var e
        ^ ") : (" ^ DBType.typeKindToString t ^ ")"
    | List e ->
        "["
        ^ List.fold_left (fun x b -> x ^ " " ^ exprToString' typeVar var b) "" e
        ^ "]"
  in
  exprToString' 1 16

let rec exprListToString = function
  | [] -> ""
  | x :: xs -> exprToString x ^ "\n" ^ exprListToString xs
