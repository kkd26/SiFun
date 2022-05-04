open DBType

(** Weak prenex conversion *)
let rec pr (tk : typeGenre) : polyType =
  let pr' =
    let tk = normalize tk in
    match tk with
    | Poly p ->
        let a, r = p in
        let b, r2 = pr (Rho r) in
        Poly (a + b, r2)
    | Rho r -> (
        match r with
        | RhoFun (p1, p2) ->
            let a2, r2 = pr (Poly p2) in
            let p1 = shiftPoly a2 0 p1 in
            let p2 = (0, r2) in
            Poly (a2, RhoFun (p1, p2))
        | RhoPair (p1, p2) ->
            let a2, r2 = pr (Poly p2) in
            let p1 = shiftPoly a2 0 p1 in
            let p2 = (0, r2) in
            Poly (a2, RhoPair (p1, p2))
        | RhoList l -> Rho (RhoList (pr (Poly l)))
        (* The last case won't be reached, as Rho(RhoMono m) gets normalized into Mono m *)
        | RhoMono _ -> failwith "not possible")
    | Mono _ -> tk
  in

  typeGenreToPoly (normalize pr')
