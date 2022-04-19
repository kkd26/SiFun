open DBType
open TermCtx

type substitution = (typeVar * typeGenre) list

let emptySubst : substitution = []

let rec substituteMono (tk : typeGenre) (x : typeVar) (target : monoType) :
    typeGenre =
  match target with
  | FreshVar y -> if x = y then tk else Mono target
  | Fun (m1, m2) ->
      Rho
        (RhoFun
           ( typeGenreToPoly (substituteMono tk x m1),
             typeGenreToPoly (substituteMono tk x m2) ))
  | Pair (m1, m2) ->
      Rho
        (RhoPair
           ( typeGenreToPoly (substituteMono tk x m1),
             typeGenreToPoly (substituteMono tk x m2) ))
  | List m -> Rho (RhoList (typeGenreToPoly (substituteMono tk x m)))
  | t -> Mono t

and substituteRho (tk : typeGenre) (x : typeVar) (target : rhoType) : typeGenre =
  match target with
  | RhoMono m -> substituteMono tk x m
  | RhoFun (p1, p2) ->
      Rho
        (RhoFun
           ( typeGenreToPoly (substitutePoly tk x p1),
             typeGenreToPoly (substitutePoly tk x p2) ))
  | RhoPair (p1, p2) ->
      Rho
        (RhoPair
           ( typeGenreToPoly (substitutePoly tk x p1),
             typeGenreToPoly (substitutePoly tk x p2) ))
  | RhoList p -> Rho (RhoList (typeGenreToPoly (substitutePoly tk x p)))

and substitutePoly (tk : typeGenre) (x : typeVar) (target : polyType) : typeGenre
    =
  let a, r = target in
  match substituteRho tk x r with
  | Poly (a', r') -> Poly (a + a', r')
  | Rho r -> Poly (a, r)
  | Mono m -> Poly (a, RhoMono m)

let substitute (tk : typeGenre) (v : typeVar) = function
  | Mono m -> normalize (substituteMono tk v m)
  | Rho r -> normalize (substituteRho tk v r)
  | Poly p -> normalize (substitutePoly tk v p)

let applySubstToTypeGenre (s : substitution) (t : typeGenre) : typeGenre =
  List.fold_right (fun (x, s) -> substitute s x) s t

let applySubstToCtx s (ctx : termCtx) : termCtx =
  List.map (applySubstToTypeGenre s) ctx

let combineSubst (s1 : substitution) (s2 : substitution) : substitution =
  List.map (fun (x, t) -> (x, applySubstToTypeGenre s1 t)) (s1 @ s2)

let combineSubstUnique (s1 : substitution) (s2 : substitution) : substitution =
  let s3 =
    List.filter (fun (y, _) -> List.for_all (fun (x, _) -> x <> y) s2) s1
  in
  let s4 =
    List.filter (fun (y, _) -> List.for_all (fun (x, _) -> x <> y) s1) s2
  in
  s3 @ s4

let substFromList list : substitution =
  let n = List.length list in
  let range = List.init n (fun x -> x) in
  List.combine range list

let substToString : substitution -> string =
  let elemToString a (x, t) =
    a ^ string_of_int x ^ ":(" ^ typeGenreToString t ^ ") "
  in
  List.fold_left elemToString "e "
