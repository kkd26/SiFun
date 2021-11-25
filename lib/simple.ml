open Debruijn

let isValue = function
  | Int _ -> true
  | Var _ -> true
  | Bool _ -> true
  | Unit -> true
  | Fun _ -> true
  | _ -> false

let rec shift i c = function
  | Int n -> Int n
  | Var n -> if n >= c then Var (n + i) else Var n
  | Bool b -> Bool b
  | Unit -> Unit
  | Pair (e1, e2) -> Pair (shift i c e1, shift i c e2)
  | Fst e -> Fst (shift i c e)
  | Snd e -> Fst (shift i c e)
  | Fun e -> Fun (shift i (c + 1) e)
  | App (e1, e2) -> App (shift i c e1, shift i c e2)
  | _ -> failwith "No reduction"

let rec subst e n = function
  | Int n -> Int n
  | Var m -> if n = m then e else Var m
  | Bool b -> Bool b
  | Unit -> Unit
  | Pair (e1, e2) -> Pair (subst e n e1, subst e n e2)
  | Fst e1 -> Fst (subst e n e1)
  | Snd e1 -> Fst (subst e n e1)
  | Fun e1 -> Fun (subst (shift 1 0 e) (n + 1) e1)
  | App (e1, e2) -> App (subst e n e1, subst e n e2)
  | _ -> failwith "No subst"

let rec reduce = function
  | Int _ -> None
  | Var _ -> None
  | Bool _ -> None
  | Unit -> None
  | Pair (e1, e2) -> (
    match reduce e1 with
    | Some e -> Some (Pair (e, e2))
    | None -> (
      match reduce e2 with Some e -> Some (Pair (e1, e)) | None -> None ) )
  | Fst e1 -> ( match e1 with Pair (e1, _) -> Some e1 | _ -> None )
  | Snd e1 -> ( match e1 with Pair (_, e2) -> Some e2 | _ -> None )
  | Fun _ -> None
  | App (e1, e2) -> (
    match e1 with
    | Fun e -> (
        if isValue e2 then Some (shift (-1) 0 (subst (shift 1 0 e2) 0 e))
        else match reduce e2 with Some e -> Some (App (e1, e)) | None -> None )
    | _ -> (
      match reduce e1 with Some e -> Some (App (e, e2)) | None -> None ) )
  | _ -> failwith "No reduction"

let doStep e = match reduce e with Some e -> e | None -> e
let rec reduceAll e = match reduce e with Some e -> reduceAll e | None -> e
