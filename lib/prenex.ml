open DBType

(** Weak prenex conversion *)
let rec pr (tk : typeKind) : polyType =
  let pr' =
    let tk = normalize tk in
    match tk with
    | Poly p ->
        let a, r = p in
        let b, r2 = pr (Rho r) in
        Poly (a + b, r2)
    | Rho r -> (
        match r with
        | F (p1, p2) ->
            let a2, r2 = pr (Poly p2) in
            let p1 = shiftPoly a2 0 p1 in
            let p2 = (0, r2) in
            Poly (a2, F (p1, p2))
        | P (p1, p2) ->
            let a2, r2 = pr (Poly p2) in
            let p1 = shiftPoly a2 0 p1 in
            let p2 = (0, r2) in
            Poly (a2, P (p1, p2))
        | T _ -> failwith "not possible")
    | Mono _ -> tk
  in

  typeKindToPoly (normalize pr')
