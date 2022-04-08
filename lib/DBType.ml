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
  | T of monoType
  | F of polyType * polyType
  | P of polyType * polyType
  | L of polyType

(** Kind of a type - monoType, rhoType, polyType *)
type typeKind = Mono of monoType | Rho of rhoType | Poly of polyType

exception DBTypeException of string

let rec monoTypeToString' var = function
  | Var v -> string_of_int v
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
  | T m -> "R[" ^ monoTypeToString' var m ^ "]"
  | F (p1, p2) ->
      "R[" ^ polyToString' var p1 ^ "->" ^ polyToString' var p2 ^ "]"
  | P (p1, p2) -> "R[" ^ polyToString' var p1 ^ "," ^ polyToString' var p2 ^ "]"
  | L p -> "R[" ^ polyToString' var p ^ "]"

and polyToString' var ((a, r) : polyType) =
  "(" ^ string_of_int a ^ "," ^ rhoToString' var r ^ ")"

let monoTypeToString = monoTypeToString' 1

let rhoToString = rhoToString' 1

let polyToString = polyToString' 1

let typeKindToString = function
  | Mono m -> monoTypeToString m
  | Rho r -> rhoToString r
  | Poly p -> polyToString p

let typeKindToMono = function
  | Mono m -> m
  | Rho (T m) -> m
  | Poly (0, T m) -> m
  | tk ->
      raise
        (DBTypeException
           ("Conversion tk to mono not possible " ^ typeKindToString tk))

let typeKindToRho = function
  | Mono m -> T m
  | Rho r -> r
  | Poly (0, r) -> r
  | _ -> raise (DBTypeException "Conversion tk to rho not possible")

let typeKindToPoly = function Poly p -> p | t -> (0, typeKindToRho t)

(** Return normalized (simplified) typeKind *)
let rec normalize = function
  | Mono m -> Mono m
  | Rho (T m) -> Mono m
  | Rho (F (p1, p2)) -> (
      let tk1 = normalize (Poly p1) in
      let tk2 = normalize (Poly p2) in
      match (tk1, tk2) with
      | Mono m1, Mono m2 -> normalize (Mono (Fun (m1, m2)))
      | _ -> Rho (F (typeKindToPoly tk1, typeKindToPoly tk2)))
  | Rho (P (p1, p2)) -> (
      let tk1 = normalize (Poly p1) in
      let tk2 = normalize (Poly p2) in
      match (tk1, tk2) with
      | Mono m1, Mono m2 -> normalize (Mono (Pair (m1, m2)))
      | _ -> Rho (P (typeKindToPoly tk1, typeKindToPoly tk2)))
  | Rho (L p) -> (
      let tk = normalize (Poly p) in
      match tk with
      | Mono m -> normalize (Mono (List m))
      | _ -> Rho (L (typeKindToPoly tk)))
  | Poly (0, r) -> normalize (Rho r)
  | Poly (a, r) -> Poly (a, typeKindToRho (normalize (Rho r)))

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
  if isMonoType typeExpr then T (monoTypeToDeBruijn' termCtx typeExpr)
  else
    match typeExpr with
    | Fun (t1, t2) ->
        F (polyTypeToDeBruijn' termCtx t1, polyTypeToDeBruijn' termCtx t2)
    | Pair (t1, t2) ->
        P (polyTypeToDeBruijn' termCtx t1, polyTypeToDeBruijn' termCtx t2)
    | List t -> L (polyTypeToDeBruijn' termCtx t)
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
let monoTypeToDeBruijn termCtx (typeExpr : Type.monoType) : typeKind =
  Mono (monoTypeToDeBruijn' termCtx typeExpr)

(** Converts a rhoType into the deBruijn notation *)
let rhoTypeToDeBruijn termCtx (typeExpr : Type.monoType) : typeKind =
  Rho (rhoTypeToDeBruijn' termCtx typeExpr)

(** Converts a polyType into the deBruijn notation *)
let polyTypeToDeBruijn termCtx (typeExpr : Type.monoType) : typeKind =
  Poly (polyTypeToDeBruijn' termCtx typeExpr)

(** Converts a type expression into the deBruijn notation *)
let typeToDeBruijn termCtx typeExpr : typeKind =
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
  | T m -> T (shiftMono i n m)
  | F (p1, p2) -> F (shiftPoly i n p1, shiftPoly i n p2)
  | P (p1, p2) -> P (shiftPoly i n p1, shiftPoly i n p2)
  | L p -> L (shiftPoly i n p)

(** Shifts polyType variables by `i` *)
and shiftPoly (i : typeVar) (n : typeVar) = function
  | a, r -> (a, shiftRho i (n + a) r)

let shiftType (i : typeVar) (n : typeVar) = function
  | Mono m -> Mono (shiftMono i n m)
  | Rho r -> Rho (shiftRho i n r)
  | Poly p -> Poly (shiftPoly i n p)

(** Substitutes `monoType` under `Var n` inside a monoType expression *)
let rec substMono (typeKind : typeKind) (n : typeVar) (m : monoType) =
  match m with
  | (Int | Bool | Unit | FreshVar _) as monoType -> Mono monoType
  | Var n' -> if n = n' then typeKind else Mono m
  | Pair (m1, m2) ->
      Rho
        (P
           ( typeKindToPoly (substMono typeKind n m1),
             typeKindToPoly (substMono typeKind n m2) ))
  | Fun (m1, m2) ->
      Rho
        (F
           ( typeKindToPoly (substMono typeKind n m1),
             typeKindToPoly (substMono typeKind n m2) ))
  | List m -> Rho (L (typeKindToPoly (substMono typeKind n m)))

(** Substitutes `monoType` under `Var n` inside a rhoType expression *)
and substRho (typeKind : typeKind) (n : typeVar) (r : rhoType) =
  match r with
  | T m1 -> substMono typeKind n m1
  | F (p1, p2) ->
      Rho
        (F
           ( typeKindToPoly (substPoly typeKind n p1),
             typeKindToPoly (substPoly typeKind n p2) ))
  | P (p1, p2) ->
      Rho
        (P
           ( typeKindToPoly (substPoly typeKind n p1),
             typeKindToPoly (substPoly typeKind n p2) ))
  | L p -> Rho (L (typeKindToPoly (substPoly typeKind n p)))

(** Substitutes `monoType` under `Var n` inside a polyType expression *)
and substPoly (typeKind : typeKind) (n : typeVar) (p : polyType) =
  let a, r = p in
  match substRho (shiftType a 0 typeKind) (n + a) r with
  | Poly (a', r') -> Poly (a + a', r')
  | Rho r -> Poly (a, r)
  | Mono m -> Poly (a, T m)

(** Substitutes `monoType` under `Var n` inside a type expression *)
let substType (typeKind : typeKind) (n : typeVar) = function
  | Mono m -> normalize (substMono typeKind n m)
  | Rho r -> normalize (substRho typeKind n r)
  | Poly p -> normalize (substPoly typeKind n p)

let applyType (typeKind : typeKind) (tk : typeKind) =
  let p = typeKindToPoly tk in
  let a, r = p in
  let p =
    match a with
    | 0 -> Poly p
    | a ->
        Poly
          (a - 1, typeKindToRho (shiftType (-1) a (substRho typeKind (a - 1) r)))
  in
  normalize p

let incChar c = String.make 1 (Char.chr (c + Char.code 'a'))

let getListType t =
  let t = normalize t in
  match t with
  | Rho (L p) -> Poly p
  | Mono (List m) -> Mono m
  | _ -> failwith "not a list"

let tkToList tk =
  let tk = normalize tk in
  match tk with
  | Mono m -> Mono (List m)
  | Rho r -> Rho (L (0, r))
  | Poly p -> Rho (L p)
