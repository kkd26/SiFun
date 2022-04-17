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
  | FunType of DBType.typeGenre * expr
  | TypeApp of expr * DBType.typeGenre
  | Lam of expr
  | Annot of expr * DBType.typeGenre
  | List of expr list
  | Head of expr
  | Tail of expr

exception DBAstException of string

let emptyEnvErrorMessage msg var =
  Printf.sprintf "%s - unbound value %s" msg var

let emptyEnv msg varName =
  raise (DBAstException (emptyEnvErrorMessage msg varName))

let update env varName lookupVar =
  if lookupVar = varName then 0 else 1 + env lookupVar

let toDeBruijn =
  let rec toDeBruijn' (env : Ast.var -> var) termEnv : Ast.expr -> expr =
    function
    | Int i -> Int i
    | Var v -> Var (env v)
    | Bool b -> Bool b
    | Unit -> Unit
    | Pair (e1, e2) ->
        Pair (toDeBruijn' env termEnv e1, toDeBruijn' env termEnv e2)
    | Fst e -> Fst (toDeBruijn' env termEnv e)
    | Snd e -> Snd (toDeBruijn' env termEnv e)
    | Fun (v, e) ->
        let newEnv = update env v in
        Fun (toDeBruijn' newEnv termEnv e)
    | App (e1, e2) ->
        App (toDeBruijn' env termEnv e1, toDeBruijn' env termEnv e2)
    | FunType (v, t, e) ->
        let newEnv = update env v in
        let tk = DBType.typeToDeBruijn termEnv t in
        FunType (tk, toDeBruijn' newEnv termEnv e)
    | TypeApp (e, t) ->
        let tk = DBType.typeToDeBruijn termEnv t in
        TypeApp (toDeBruijn' env termEnv e, tk)
    | Lam (v, e) ->
        let newTermEnv = update termEnv v in
        Lam (toDeBruijn' env newTermEnv e)
    | Annot (e, t) ->
        let tk = DBType.typeToDeBruijn termEnv t in
        Annot (toDeBruijn' env termEnv e, tk)
    | List e -> List (List.map (toDeBruijn' env termEnv) e)
    | Head e -> Head (toDeBruijn' env termEnv e)
    | Tail e -> Tail (toDeBruijn' env termEnv e)
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
        "fn " ^ DBType.incChar var ^ " : " ^ DBType.typeGenreToString t ^ " => "
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
        ^ ") {" ^ DBType.typeGenreToString t ^ "}"
    | Lam e ->
        "lam " ^ DBType.incChar typeVar ^ "."
        ^ exprToString' (typeVar + 1) var e
    | Annot (e, t) ->
        "("
        ^ exprToString' typeVar var e
        ^ ") : (" ^ DBType.typeGenreToString t ^ ")"
    | List e ->
        "["
        ^ List.fold_left (fun x b -> x ^ " " ^ exprToString' typeVar var b) "" e
        ^ "]"
    | Head e -> "hd " ^ exprToString' typeVar var e
    | Tail e -> "tl " ^ exprToString' typeVar var e
  in
  exprToString' 1 16

let rec exprListToString = function
  | [] -> ""
  | x :: xs -> exprToString x ^ "\n" ^ exprListToString xs
