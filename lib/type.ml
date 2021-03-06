type typeVar = string

type monoType =
  | Var of typeVar
  | Int
  | Bool
  | Unit
  | Pair of monoType * monoType
  | Fun of monoType * monoType
  | ForAll of typeVar * monoType
  | List of monoType

let forAllFromList typeVarList typeExpr =
  List.fold_right (fun v t -> ForAll (v, t)) typeVarList typeExpr

let rec typeExprToString = function
  | Var s -> "Var(" ^ s ^ ")"
  | Int -> "Int"
  | Bool -> "Bool"
  | Unit -> "Unit"
  | Pair (t1, t2) ->
      "Pair(" ^ typeExprToString t1 ^ "," ^ typeExprToString t2 ^ ")"
  | Fun (t1, t2) ->
      "Fun(" ^ typeExprToString t1 ^ "," ^ typeExprToString t2 ^ ")"
  | ForAll (s, t) -> "ForAll(" ^ s ^ "," ^ typeExprToString t ^ ")"
  | List m -> "List(" ^ typeExprToString m ^ ")"
