open DBType
open TypeCtx

type substitution = (typeVar * typeKind) list

let emptySubst : substitution = []

let rec substituteMono (tk : typeKind) (x : typeVar) (target : monoType) :
    monoType =
  match target with
  | FreshVar y -> if x = y then typeKindToMono tk else target
  | Fun (m1, m2) -> Fun (substituteMono tk x m1, substituteMono tk x m2)
  | Pair (m1, m2) -> Pair (substituteMono tk x m1, substituteMono tk x m2)
  | t -> t

let rec substituteRho (tk : typeKind) (x : typeVar) (target : rhoType) : rhoType
    =
  match target with
  | T m -> T (substituteMono tk x m)
  | F (p1, p2) -> F (substitutePoly tk x p1, substitutePoly tk x p2)

and substitutePoly (tk : typeKind) (x : typeVar) (target : polyType) : polyType
    =
  let a, r = target in
  (a, substituteRho tk x r)

let substitute (tk : typeKind) (v : typeVar) = function
  | Mono m -> Mono (substituteMono tk v m)
  | Rho r -> Rho (substituteRho tk v r)
  | Poly p -> Poly (substitutePoly tk v p)

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
