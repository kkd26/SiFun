type typeVar = int

type monoType =
  | Var of typeVar (* function variables, big lambda, forall *)
  | FreshVar of typeVar (* for unification *)
  | Int
  | Bool
  | Unit
  | Pair of monoType * monoType
  | Fun of monoType * monoType
  | List of monoType

and polyType = int * rhoType

and rhoType =
  | RhoMono of monoType
  | RhoFun of polyType * polyType
  | RhoPair of polyType * polyType
  | RhoList of polyType

(** Genre of a type - monoType, rhoType, polyType *)
type typeGenre = Mono of monoType | Rho of rhoType | Poly of polyType

exception DBTypeException of string

let numToString n start range =
  let i = n / range in
  String.make 1 (Char.chr (Char.code 'a' + (start + (n mod range))))
  ^ if i <> 0 then string_of_int i else ""

let numToStringTypeVar n = numToString n 0 16
let numToStringExprVar n = numToString n 16 10

let rec printTypeVars var = function
  | 0 -> ""
  | n -> printTypeVars var (n - 1) ^ " " ^ numToStringTypeVar (n - 1 + var)

let rec monoTypeToString' var = function
  | Var v -> numToStringTypeVar (var - v - 1)
  | FreshVar v -> "f" ^ string_of_int v
  | Int -> "int"
  | Bool -> "bool"
  | Unit -> "unit"
  | Pair (m1, m2) ->
      "(" ^ monoTypeToString' var m1 ^ ", " ^ monoTypeToString' var m2 ^ ")"
  | Fun (m1, m2) ->
      let inner = monoTypeToString' var m1 in
      (match m1 with Fun (_, _) -> "(" ^ inner ^ ")" | _ -> inner)
      ^ " -> " ^ monoTypeToString' var m2
  | List m -> monoTypeToString' var m ^ " list"

and rhoToString' var = function
  | RhoMono m -> monoTypeToString' var m
  | RhoFun (p1, p2) -> polyToString' var p1 ^ " -> " ^ polyToString' var p2
  | RhoPair (p1, p2) ->
      "(" ^ polyToString' var p1 ^ ", " ^ polyToString' var p2 ^ ")"
  | RhoList p -> polyToString' var p ^ " list"

and polyToString' var ((a, r) : polyType) =
  match a with
  | 0 -> rhoToString' var r
  | n -> "forall" ^ printTypeVars var n ^ ". (" ^ rhoToString' (var + n) r ^ ")"

let monoTypeToString = monoTypeToString' 16
let rhoToString = rhoToString' 16
let polyToString = polyToString' 16

let typeGenreToString' var = function
  | Mono m -> monoTypeToString' var m
  | Rho r -> rhoToString' var r
  | Poly p -> polyToString' var p

let typeGenreToString = typeGenreToString' 0

let typeGenreToMono = function
  | Mono m -> m
  | Rho (RhoMono m) -> m
  | Poly (0, RhoMono m) -> m
  | tk ->
      raise
        (DBTypeException
           ("Conversion tk to mono not possible " ^ typeGenreToString tk))

let typeGenreToRho = function
  | Mono m -> RhoMono m
  | Rho r -> r
  | Poly (0, r) -> r
  | _ -> raise (DBTypeException "Conversion tk to rho not possible")

let typeGenreToPoly = function Poly p -> p | t -> (0, typeGenreToRho t)

(** Return normalized (simplified) typeGenre *)
let rec normalize = function
  | Mono m -> Mono m
  | Rho (RhoMono m) -> Mono m
  | Rho (RhoFun (p1, p2)) -> (
      let tk1 = normalize (Poly p1) in
      let tk2 = normalize (Poly p2) in
      match (tk1, tk2) with
      | Mono m1, Mono m2 -> normalize (Mono (Fun (m1, m2)))
      | _ -> Rho (RhoFun (typeGenreToPoly tk1, typeGenreToPoly tk2)))
  | Rho (RhoPair (p1, p2)) -> (
      let tk1 = normalize (Poly p1) in
      let tk2 = normalize (Poly p2) in
      match (tk1, tk2) with
      | Mono m1, Mono m2 -> normalize (Mono (Pair (m1, m2)))
      | _ -> Rho (RhoPair (typeGenreToPoly tk1, typeGenreToPoly tk2)))
  | Rho (RhoList p) -> (
      let tk = normalize (Poly p) in
      match tk with
      | Mono m -> normalize (Mono (List m))
      | _ -> Rho (RhoList (typeGenreToPoly tk)))
  | Poly (0, r) -> normalize (Rho r)
  | Poly (a, r) -> Poly (a, typeGenreToRho (normalize (Rho r)))

(** Updates current environment `env` and returns a new one *)
let update env newVal queryVal =
  if queryVal = newVal then 0 else 1 + env queryVal

(** Checks if a type is a monoType - i.e. doesn't have any ForAll *)
let isMonoType =
  let rec isMonoType' acc : Type.monoType -> bool = function
    | ForAll _ -> false
    | Pair (m1, m2) -> isMonoType' (isMonoType' acc m1) m2
    | Fun (m1, m2) -> isMonoType' (isMonoType' acc m1) m2
    | List m -> isMonoType' acc m
    | _ -> acc
  in
  isMonoType' true

let rec monoTypeToDeBruijn' termCtx (typeExpr : Type.monoType) : monoType =
  match typeExpr with
  | Int -> Int
  | Var v -> Var (termCtx v)
  | Bool -> Bool
  | Unit -> Unit
  | Pair (m1, m2) ->
      Pair (monoTypeToDeBruijn' termCtx m1, monoTypeToDeBruijn' termCtx m2)
  | Fun (m1, m2) ->
      Fun (monoTypeToDeBruijn' termCtx m1, monoTypeToDeBruijn' termCtx m2)
  | List m -> List (monoTypeToDeBruijn' termCtx m)
  | _ -> raise (DBTypeException "Not a monoType")

and rhoTypeToDeBruijn' termCtx (typeExpr : Type.monoType) : rhoType =
  if isMonoType typeExpr then RhoMono (monoTypeToDeBruijn' termCtx typeExpr)
  else
    match typeExpr with
    | Fun (t1, t2) ->
        RhoFun (polyTypeToDeBruijn' termCtx t1, polyTypeToDeBruijn' termCtx t2)
    | Pair (t1, t2) ->
        RhoPair (polyTypeToDeBruijn' termCtx t1, polyTypeToDeBruijn' termCtx t2)
    | List t -> RhoList (polyTypeToDeBruijn' termCtx t)
    | _ -> raise (DBTypeException "Not a rhoType")

(** Converts a polyType into the deBruijn notation *)
and polyTypeToDeBruijn' termCtx : Type.monoType -> polyType = function
  | ForAll (v, t) ->
      let newCtx = update termCtx v in
      let a, r =
        match t with
        | ForAll _ -> polyTypeToDeBruijn' newCtx t
        | _ -> (0, rhoTypeToDeBruijn' newCtx t)
      in
      (a + 1, r)
  | t ->
      let r = rhoTypeToDeBruijn' termCtx t in
      (0, r)

(** Converts a monoType into the deBruijn notation *)
let monoTypeToDeBruijn termCtx (typeExpr : Type.monoType) : typeGenre =
  Mono (monoTypeToDeBruijn' termCtx typeExpr)

(** Converts a rhoType into the deBruijn notation *)
let rhoTypeToDeBruijn termCtx (typeExpr : Type.monoType) : typeGenre =
  Rho (rhoTypeToDeBruijn' termCtx typeExpr)

(** Converts a polyType into the deBruijn notation *)
let polyTypeToDeBruijn termCtx (typeExpr : Type.monoType) : typeGenre =
  Poly (polyTypeToDeBruijn' termCtx typeExpr)

(** Converts a type expression into the deBruijn notation *)
let typeToDeBruijn termCtx typeExpr : typeGenre =
  if isMonoType typeExpr then monoTypeToDeBruijn termCtx typeExpr
  else
    match typeExpr with
    | ForAll _ -> polyTypeToDeBruijn termCtx typeExpr
    | _ -> rhoTypeToDeBruijn termCtx typeExpr

(** Shifts monoType variables by `i` *)
let rec shiftMono (i : typeVar) (n : typeVar) = function
  | (Int | Bool | Unit | FreshVar _) as m -> m
  | Var n' -> if n' >= n then Var (n' + i) else Var n'
  | Pair (m1, m2) -> Pair (shiftMono i n m1, shiftMono i n m2)
  | Fun (m1, m2) -> Fun (shiftMono i n m1, shiftMono i n m2)
  | List m -> List (shiftMono i n m)

(** Shifts rhoType variables by `i` *)
and shiftRho (i : typeVar) (n : typeVar) = function
  | RhoMono m -> RhoMono (shiftMono i n m)
  | RhoFun (p1, p2) -> RhoFun (shiftPoly i n p1, shiftPoly i n p2)
  | RhoPair (p1, p2) -> RhoPair (shiftPoly i n p1, shiftPoly i n p2)
  | RhoList p -> RhoList (shiftPoly i n p)

(** Shifts polyType variables by `i` *)
and shiftPoly (i : typeVar) (n : typeVar) = function
  | a, r -> (a, shiftRho i (n + a) r)

let shiftType (i : typeVar) (n : typeVar) = function
  | Mono m -> Mono (shiftMono i n m)
  | Rho r -> Rho (shiftRho i n r)
  | Poly p -> Poly (shiftPoly i n p)

(** Substitutes `monoType` under `Var n` inside a monoType expression *)
let rec substMono (typeGenre : typeGenre) (n : typeVar) (m : monoType) =
  match m with
  | (Int | Bool | Unit | FreshVar _) as monoType -> Mono monoType
  | Var n' -> if n = n' then typeGenre else Mono m
  | Pair (m1, m2) ->
      Rho
        (RhoPair
           ( typeGenreToPoly (substMono typeGenre n m1),
             typeGenreToPoly (substMono typeGenre n m2) ))
  | Fun (m1, m2) ->
      Rho
        (RhoFun
           ( typeGenreToPoly (substMono typeGenre n m1),
             typeGenreToPoly (substMono typeGenre n m2) ))
  | List m -> Rho (RhoList (typeGenreToPoly (substMono typeGenre n m)))

(** Substitutes `monoType` under `Var n` inside a rhoType expression *)
and substRho (typeGenre : typeGenre) (n : typeVar) (r : rhoType) =
  match r with
  | RhoMono m1 -> substMono typeGenre n m1
  | RhoFun (p1, p2) ->
      Rho
        (RhoFun
           ( typeGenreToPoly (substPoly typeGenre n p1),
             typeGenreToPoly (substPoly typeGenre n p2) ))
  | RhoPair (p1, p2) ->
      Rho
        (RhoPair
           ( typeGenreToPoly (substPoly typeGenre n p1),
             typeGenreToPoly (substPoly typeGenre n p2) ))
  | RhoList p -> Rho (RhoList (typeGenreToPoly (substPoly typeGenre n p)))

(** Substitutes `monoType` under `Var n` inside a polyType expression *)
and substPoly (typeGenre : typeGenre) (n : typeVar) (p : polyType) =
  let a, r = p in
  match substRho (shiftType a 0 typeGenre) (n + a) r with
  | Poly (a', r') -> Poly (a + a', r')
  | Rho r -> Poly (a, r)
  | Mono m -> Poly (a, RhoMono m)

(** Substitutes `monoType` under `Var n` inside a type expression *)
let substType (typeGenre : typeGenre) (n : typeVar) = function
  | Mono m -> normalize (substMono typeGenre n m)
  | Rho r -> normalize (substRho typeGenre n r)
  | Poly p -> normalize (substPoly typeGenre n p)

let applyType (typeGenre : typeGenre) (tk : typeGenre) =
  let p = typeGenreToPoly tk in
  let a, r = p in
  let p =
    match a with
    | 0 -> Poly p
    | a ->
        Poly
          ( a - 1,
            typeGenreToRho (shiftType (-1) a (substRho typeGenre (a - 1) r)) )
  in
  normalize p

let getListType t =
  let t = normalize t in
  match t with
  | Rho (RhoList p) -> Poly p
  | Mono (List m) -> Mono m
  | _ -> failwith "not a list"

let typeGenreToList tk =
  let tk = normalize tk in
  match tk with
  | Mono m -> Mono (List m)
  | Rho r -> Rho (RhoList (0, r))
  | Poly p -> Rho (RhoList p)
