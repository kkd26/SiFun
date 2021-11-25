type typeVar = string

type monoType =
  | Var of typeVar
  | Int
  | Bool
  | Unit
  | Pair of monoType * monoType
  | Fun of monoType * monoType

let numToString n =
  let i = n / 26 in
  String.make 1 (Char.chr (Char.code 'a' + (n mod 26)))
  ^ if i <> 0 then string_of_int i else ""

let rec typeExprToString = function
  | Var s -> s
  | Int -> "int"
  | Bool -> "bool"
  | Unit -> "unit"
  | Pair (t1, t2) ->
      "(" ^ typeExprToString t1 ^ ", " ^ typeExprToString t2 ^ ")"
  | Fun (t1, t2) ->
      let inner = typeExprToString t1 in
      (match t1 with Fun (_, _) -> "(" ^ inner ^ ")" | _ -> inner)
      ^ " -> " ^ typeExprToString t2
