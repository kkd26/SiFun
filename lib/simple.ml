open Debruijn

let isValue = function
  | Int _ -> true
  | Var _ -> true
  | Bool _ -> true
  | Unit -> true
  | Fun _ -> true
  | FunType _ -> true
  | Lam _ -> true
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
  | FunType (t, e) -> FunType (t, shift i (c + 1) e)
  | TypeApp (e, t) -> TypeApp (shift i c e, t)
  | Lam e -> Lam (shift i c e)

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
  | FunType (t, e1) -> FunType (t, subst (shift 1 0 e) (n + 1) e1)
  | TypeApp (e1, t) -> TypeApp (subst e n e1, t)
  | Lam e1 -> Lam (subst e n e1)

let rec shiftType i c = function
  | Pair (e1, e2) -> Pair (shiftType i c e1, shiftType i c e2)
  | Fst e -> Fst (shiftType i c e)
  | Snd e -> Fst (shiftType i c e)
  | Fun e -> Fun (shiftType i c e)
  | App (e1, e2) -> App (shiftType i c e1, shiftType i c e2)
  | FunType (p, e) -> FunType (DBType.shiftType i c p, shiftType i c e)
  | TypeApp (e, m) -> TypeApp (shiftType i c e, DBType.shiftType i c m)
  | Lam e -> Lam (shiftType i (c + 1) e)
  | e -> e

let rec substType t n = function
  | Pair (e1, e2) -> Pair (substType t n e1, substType t n e2)
  | Fst e1 -> Fst (substType t n e1)
  | Snd e1 -> Fst (substType t n e1)
  | Fun e1 -> Fun (substType t n e1)
  | App (e1, e2) -> App (substType t n e1, substType t n e2)
  | FunType (p, e1) -> FunType (DBType.substType t n p, substType t n e1)
  | TypeApp (e1, m) -> TypeApp (substType t n e1, DBType.substType t n m)
  | Lam e1 -> Lam (substType (DBType.shiftType 1 0 t) (n + 1) e1)
  | e -> e

let rec reduce = function
  | Int _ -> None
  | Var _ -> None
  | Bool _ -> None
  | Unit -> None
  | Pair (e1, e2) -> (
      match reduce e1 with
      | Some e -> Some (Pair (e, e2))
      | None -> (
          match reduce e2 with Some e -> Some (Pair (e1, e)) | None -> None))
  | Fst e1 -> ( match e1 with Pair (e1, _) -> Some e1 | _ -> None)
  | Snd e1 -> ( match e1 with Pair (_, e2) -> Some e2 | _ -> None)
  | Fun _ -> None
  | App (e1, e2) -> (
      match e1 with
      | Fun e | FunType (_, e) -> (
          if isValue e2 then Some (shift (-1) 0 (subst (shift 1 0 e2) 0 e))
          else
            match reduce e2 with Some e -> Some (App (e1, e)) | None -> None)
      | _ -> (
          match reduce e1 with Some e -> Some (App (e, e2)) | None -> None))
  | FunType _ -> None
  | TypeApp (e1, t) -> (
      match e1 with
      | Lam e -> Some (shiftType (-1) 0 (substType t 0 e))
      | _ -> (
          match reduce e1 with Some e -> Some (TypeApp (e, t)) | None -> None))
  | Lam _ -> None

let doStep e = match reduce e with Some e -> e | None -> e
let rec reduceAll e = match reduce e with Some e -> reduceAll e | None -> e
