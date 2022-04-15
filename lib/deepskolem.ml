open DBType
open Prenex
open Unify
open Direction
open State
open Subst

let returnNormalized (sub, typ) =
  let open IntState in
  return (sub, normalize typ)

(** Deep skolemisation *)
let rec dsk (tk : typeGenre) (p' : polyType) =
  let tk = normalize tk in
  let _, r = pr (Poly p') in
  dsk' tk r

and dsk' (tk : typeGenre) (r : rhoType) =
  let tk = normalize tk in
  let tk' = normalize (Rho r) in
  let open IntState in
  match (tk, tk') with
  | Mono t, Mono t' -> unifyMono t t' >>= fun s -> return s
  | Rho (RhoFun (p1, p2)), Rho (RhoFun (p3, p4)) ->
      dsk (Poly p3) p1 >>= fun s1 ->
      dsk (Poly p2) p4 >>= fun s2 -> return (combineSubst s1 s2)
  | Rho (RhoPair (p1, p2)), Rho (RhoPair (p3, p4)) ->
      dsk (Poly p1) p3 >>= fun s1 ->
      dsk (Poly p2) p4 >>= fun s2 -> return (combineSubst s1 s2)
  | Poly _, r' ->
      inst Infer tk >>= fun (s1, tx) ->
      unifyOne tx r' >>= fun s2 -> return (combineSubst s1 s2)
  | _, _ -> failwith "skolem error"

(** Instantiation, create fresh variables and remove type schema *)
and inst (d : direction) (p : typeGenre) : (substitution * typeGenre) IntState.t =
  let open IntState in
  match d with
  | Infer -> (
      let a, r = typeGenreToPoly (normalize p) in
      match a with
      | 0 -> returnNormalized (emptySubst, Rho r)
      | _ -> freshName >>= fun x -> inst d (applyType (Mono (FreshVar x)) p))
  | Check tk ->
      dsk p (typeGenreToPoly tk) >>= fun s ->
      returnNormalized (s, applySubstToTypeGenre s p)
