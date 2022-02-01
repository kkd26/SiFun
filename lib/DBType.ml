type typeVar = int

type monoType =
  | Var of typeVar (* function variables, big lambda, forall *)
  | FreshVar of typeVar (* for unification *)
  | Int
  | Bool
  | Unit
  | Pair of monoType * monoType
  | Fun of monoType * monoType
  | ForAll of monoType

let update env x y = if y = x then 0 else 1 + env y

let rec toDeBruijn typeCtx : Type.monoType -> monoType = function
  | Int -> Int
  | Var v -> Var (typeCtx v)
  | Bool -> Bool
  | Unit -> Unit
  | Pair (t1, t2) -> Pair (toDeBruijn typeCtx t1, toDeBruijn typeCtx t2)
  | Fun (t1, t2) -> Fun (toDeBruijn typeCtx t1, toDeBruijn typeCtx t2)
  | ForAll (v, t) ->
      let newEnv = update typeCtx v in
      ForAll (toDeBruijn newEnv t)

let rec shift i c = function
  | (Int | Bool | Unit | FreshVar _) as t -> t
  | Var n -> if n >= c then Var (n + i) else Var n
  | Pair (t1, t2) -> Pair (shift i c t1, shift i c t2)
  | Fun (t1, t2) -> Fun (shift i c t1, shift i c t2)
  | ForAll t -> ForAll (shift i (c + 1) t)

let rec subst t n = function
  | (Int | Bool | Unit | FreshVar _) as t -> t
  | Var m -> if n = m then t else Var m
  | Pair (t1, t2) -> Pair (subst t n t1, subst t n t2)
  | Fun (t1, t2) -> Fun (subst t n t1, subst t n t2)
  | ForAll t1 -> ForAll (subst (shift 1 0 t) (n + 1) t1)

let verifyType =
  let rec verifyType' typeCtx = function
    | Var v -> v < typeCtx
    | FreshVar _ | Int | Bool | Unit -> true
    | Pair (e1, e2) -> verifyType' typeCtx e1 && verifyType' typeCtx e2
    | Fun (e1, e2) -> verifyType' typeCtx e1 && verifyType' typeCtx e2
    | ForAll t -> verifyType' (typeCtx + 1) t
  in
  verifyType' 0

let incChar c = String.make 1 (Char.chr (c + Char.code 'a'))

let rec typeExprToString' var = function
  | Var s -> incChar (var - s - 1)
  | FreshVar s -> "f" ^ string_of_int s
  | Int -> "int"
  | Bool -> "bool"
  | Unit -> "unit"
  | Pair (t1, t2) ->
      "(" ^ typeExprToString' var t1 ^ ", " ^ typeExprToString' var t2 ^ ")"
  | Fun (t1, t2) ->
      let inner = typeExprToString' var t1 in
      (match t1 with Fun (_, _) -> "(" ^ inner ^ ")" | _ -> inner)
      ^ " -> " ^ typeExprToString' var t2
  | ForAll t ->
      "forall " ^ incChar var ^ ".(" ^ typeExprToString' (var + 1) t ^ ")"

let typeExprToString = typeExprToString' 0
