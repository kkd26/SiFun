open DBType
open TypeCtx

type substitution = (typeVar * typeKind) list

let emptySubst : substitution = []

let rec substituteMono (tk : typeKind) (x : typeVar) (target : monoType) :
    typeKind =
  match target with
  | FreshVar y -> if x = y then tk else Mono target
  | Fun (m1, m2) ->
      Rho
        (F
           ( typeKindToPoly (substituteMono tk x m1),
             typeKindToPoly (substituteMono tk x m2) ))
  | Pair (m1, m2) ->
      Rho
        (P
           ( typeKindToPoly (substituteMono tk x m1),
             typeKindToPoly (substituteMono tk x m2) ))
  | t -> Mono t

and substituteRho (tk : typeKind) (x : typeVar) (target : rhoType) : typeKind =
  match target with
  | T m -> substituteMono tk x m
  | F (p1, p2) ->
      Rho
        (F
           ( typeKindToPoly (substitutePoly tk x p1),
             typeKindToPoly (substitutePoly tk x p2) ))
  | P (p1, p2) ->
      Rho
        (P
           ( typeKindToPoly (substitutePoly tk x p1),
             typeKindToPoly (substitutePoly tk x p2) ))

and substitutePoly (tk : typeKind) (x : typeVar) (target : polyType) : typeKind
    =
  let a, r = target in
  match substituteRho tk x r with
  | Poly (a', r') -> Poly (a + a', r')
  | Rho r -> Poly (a, r)
  | Mono m -> Poly (a, T m)

let substitute (tk : typeKind) (v : typeVar) = function
  | Mono m -> normalize (substituteMono tk v m)
  | Rho r -> normalize (substituteRho tk v r)
  | Poly p -> normalize (substitutePoly tk v p)

let applySubstToTypeKind (s : substitution) (t : typeKind) : typeKind =
  List.fold_right (fun (x, s) -> substitute s x) s t

let applySubstToCtx s (ctx : typeCtx) : typeCtx =
  List.map (applySubstToTypeKind s) ctx

let combineSubst s1 s2 : substitution =
  List.map (fun (x, t) -> (x, applySubstToTypeKind s1 t)) (s1 @ s2)

let substFromList list : substitution =
  let n = List.length list in
  let range = List.init n (fun x -> x) in
  List.combine range list

let substToString =
  let elemToString a (x, t) =
    a ^ string_of_int x ^ ":(" ^ typeKindToString t ^ ") "
  in
  List.fold_left elemToString "e "
