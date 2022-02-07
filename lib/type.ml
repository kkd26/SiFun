type typeVar = string

type monoType =
  | Var of typeVar
  | Int
  | Bool
  | Unit
  | Pair of monoType * monoType
  | Fun of monoType * monoType
  | ForAll of typeVar * monoType

let forAllFromList typeVarList typeExpr =
  List.fold_right (fun v t -> ForAll (v, t)) typeVarList typeExpr

let numToString n =
  let i = n / 26 in
  String.make 1 (Char.chr (Char.code 'a' + (n mod 26)))
  ^ if i <> 0 then string_of_int i else ""

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
