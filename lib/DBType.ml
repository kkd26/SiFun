type typeVar = int

type monoType =
  | Var of typeVar (* function variables, big lambda, forall *)
  | FreshVar of typeVar (* for unification *)
  | Int
  | Bool
  | Unit
  | Pair of monoType * monoType
  | Fun of monoType * monoType

and polyType = int * rhoType
and rhoType = T of monoType | F of polyType * polyType

(** Kind of a type - monoType, rhoType, polyType *)
type typeKind = Mono of monoType | Rho of rhoType | Poly of polyType

exception DBTypeException of string

let rec monoTypeToString' var = function
  | Var s -> string_of_int s
  | FreshVar s -> "f" ^ string_of_int s
  | Int -> "int"
  | Bool -> "bool"
  | Unit -> "unit"
  | Pair (t1, t2) ->
      "(" ^ monoTypeToString' var t1 ^ ", " ^ monoTypeToString' var t2 ^ ")"
  | Fun (t1, t2) ->
      let inner = monoTypeToString' var t1 in
      (match t1 with Fun (_, _) -> "(" ^ inner ^ ")" | _ -> inner)
      ^ " -> " ^ monoTypeToString' var t2

let rec rhoToString' var = function
  | T t -> "R[" ^ monoTypeToString' var t ^ "]"
  | F (s1, s2) ->
      "R[" ^ polyToString' var s1 ^ "->" ^ polyToString' var s2 ^ "]"

and polyToString' var (a, r) =
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

let rec normalize = function
  | Mono m -> Mono m
  | Rho (T m) -> Mono m
  | Rho (F (p1, p2)) -> (
      let tk1 = normalize (Poly p1) in
      let tk2 = normalize (Poly p2) in
      match (tk1, tk2) with
      | Mono m1, Mono m2 -> normalize (Mono (Fun (m1, m2)))
      | _ -> Rho (F (typeKindToPoly tk1, typeKindToPoly tk2)))
  | Poly (0, r) -> normalize (Rho r)
  | Poly (a, r) -> Poly (a, typeKindToRho (normalize (Rho r)))

(** Updates current environment `env` and returns a new one *)
let update env newVal queryVal =
  if queryVal = newVal then 0 else 1 + env queryVal

(** Checks if a type is a monoType - i.e. doesn't have any ForAll *)
let isMonoType =
  let rec isMonoType' acc : Type.monoType -> bool = function
    | ForAll _ -> false
    | Pair (t1, t2) -> isMonoType' (isMonoType' acc t1) t2
    | Fun (t1, t2) -> isMonoType' (isMonoType' acc t1) t2
    | _ -> acc
  in
  isMonoType' true

let rec monoTypeToDeBruijn' typeCtx (typeExpr : Type.monoType) : monoType =
  match typeExpr with
  | Int -> Int
  | Var v -> Var (typeCtx v)
  | Bool -> Bool
  | Unit -> Unit
  | Pair (t1, t2) ->
      Pair (monoTypeToDeBruijn' typeCtx t1, monoTypeToDeBruijn' typeCtx t2)
  | Fun (t1, t2) ->
      Fun (monoTypeToDeBruijn' typeCtx t1, monoTypeToDeBruijn' typeCtx t2)
  | _ -> raise (DBTypeException "Not a monoType")

let rec rhoTypeToDeBruijn' typeCtx (typeExpr : Type.monoType) : rhoType =
  if isMonoType typeExpr then T (monoTypeToDeBruijn' typeCtx typeExpr)
  else
    match typeExpr with
    | Fun (t1, t2) ->
        F (polyTypeToDeBruijn' typeCtx t1, polyTypeToDeBruijn' typeCtx t2)
    | _ -> raise (DBTypeException "Not a rhoType")

(** Converts a polyType into the deBruijn notation *)
and polyTypeToDeBruijn' typeCtx : Type.monoType -> polyType = function
  | ForAll (v, t) ->
      let newCtx = update typeCtx v in
      let a, r =
        match t with
        | ForAll _ -> polyTypeToDeBruijn' newCtx t
        | _ -> (0, rhoTypeToDeBruijn' newCtx t)
      in
      (a + 1, r)
  | t ->
      let r = rhoTypeToDeBruijn' typeCtx t in
      (0, r)

(** Converts a monoType into the deBruijn notation *)
let monoTypeToDeBruijn typeCtx (typeExpr : Type.monoType) : typeKind =
  Mono (monoTypeToDeBruijn' typeCtx typeExpr)

(** Converts a rhoType into the deBruijn notation *)
let rhoTypeToDeBruijn typeCtx (typeExpr : Type.monoType) : typeKind =
  Rho (rhoTypeToDeBruijn' typeCtx typeExpr)

(** Converts a polyType into the deBruijn notation *)
let polyTypeToDeBruijn typeCtx (typeExpr : Type.monoType) : typeKind =
  Poly (polyTypeToDeBruijn' typeCtx typeExpr)

(** Converts a type expression into the deBruijn notation *)
let typeToDeBruijn typeCtx typeExpr : typeKind =
  if isMonoType typeExpr then monoTypeToDeBruijn typeCtx typeExpr
  else
    match typeExpr with
    | ForAll _ -> polyTypeToDeBruijn typeCtx typeExpr
    | _ -> rhoTypeToDeBruijn typeCtx typeExpr

(** Shifts monoType variables by `i` *)
let rec shiftMono (i : typeVar) (n : typeVar) = function
  | (Int | Bool | Unit | FreshVar _) as m -> m
  | Var n' -> if n' >= n then Var (n' + i) else Var n'
  | Pair (m1, m2) -> Pair (shiftMono i n m1, shiftMono i n m2)
  | Fun (m1, m2) -> Fun (shiftMono i n m1, shiftMono i n m2)

(** Shifts rhoType variables by `i` *)
let rec shiftRho (i : typeVar) (n : typeVar) = function
  | T m -> T (shiftMono i n m)
  | F (p1, p2) -> F (shiftPoly i n p1, shiftPoly i n p2)

(** Shifts polyType variables by `i` *)
and shiftPoly (i : typeVar) (n : typeVar) = function
  | a, r -> (a, shiftRho i (n + a) r)

let shiftType (i : typeVar) (n : typeVar) = function
  | Mono m -> Mono (shiftMono i n m)
  | Rho r -> Rho (shiftRho i n r)
  | Poly p -> Poly (shiftPoly i n p)

(** Substitutes `monoType` under `Var n` inside a monoType expression *)
let rec substMono (monoType : monoType) (n : typeVar) (m : monoType) =
  match m with
  | (Int | Bool | Unit | FreshVar _) as monoType -> monoType
  | Var n' -> if n = n' then monoType else Var n'
  | Pair (m1, m2) -> Pair (substMono monoType n m1, substMono monoType n m2)
  | Fun (m1, m2) -> Fun (substMono monoType n m1, substMono monoType n m2)

(** Substitutes `monoType` under `Var n` inside a rhoType expression *)
let rec substRho (monoType : monoType) (n : typeVar) (r : rhoType) =
  match r with
  | T m1 -> T (substMono monoType n m1)
  | F (p1, p2) -> F (substPoly monoType n p1, substPoly monoType n p2)

(** Substitutes `monoType` under `Var n` inside a polyType expression *)
and substPoly (monoType : monoType) (n : typeVar) (p : polyType) =
  let a, r = p in
  (a, substRho (shiftMono a 0 monoType) (n + a) r)

(** Substitutes `monoType` under `Var n` inside a type expression *)
let substType (monoType : monoType) (n : typeVar) = function
  | Mono m -> Mono (substMono monoType n m)
  | Rho r -> Rho (substRho monoType n r)
  | Poly p -> Poly (substPoly monoType n p)

let incChar c = String.make 1 (Char.chr (c + Char.code 'a'))
