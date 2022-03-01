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
let rec dsk (tk : typeKind) (p' : polyType) =
  let tk = normalize tk in
  let _, r = pr (Poly p') in
  dsk' tk r

and dsk' (tk : typeKind) (r : rhoType) =
  let tk = normalize tk in
  let tk' = normalize (Rho r) in
  let open IntState in
  match (tk, tk') with
  | Mono t, Mono t' -> unifyMono t t' >>= fun s -> return s
  | Rho (F (p1, p2)), Rho (F (p3, p4)) ->
      dsk (Poly p3) p1 >>= fun s1 ->
      dsk (Poly p2) p4 >>= fun s2 -> return (combineSubst s1 s2)
  | Rho (P (p1, p2)), Rho (P (p3, p4)) ->
      dsk (Poly p1) p3 >>= fun s1 ->
      dsk (Poly p2) p4 >>= fun s2 -> return (combineSubst s1 s2)
  | Poly _, r' ->
      inst Infer tk >>= fun (s1, tx) ->
      unifyOne tx r' >>= fun s2 -> return (combineSubst s1 s2)
  | _, _ -> failwith "skolem error"

(** Instantiation, create fresh variables and remove type schema *)
and inst (d : direction) (p : typeKind) : (substitution * typeKind) IntState.t =
  let open IntState in
  match d with
  | Infer -> (
      let a, r = typeKindToPoly (normalize p) in
      match a with
      | 0 -> returnNormalized (emptySubst, Rho r)
      | _ -> freshName >>= fun x -> inst d (applyType (Mono (FreshVar x)) p))
  | Check tk ->
      dsk p (typeKindToPoly tk) >>= fun s ->
      returnNormalized (s, applySubstToTypeKind s p)
