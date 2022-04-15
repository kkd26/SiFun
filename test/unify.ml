open Sifun
open DBType
open Subst
open Unify
open OUnit2

let basicTypes =
  [
    Int;
    Bool;
    Unit;
    FreshVar 0;
    FreshVar 1;
    FreshVar 2;
    FreshVar 3;
    FreshVar 4;
    FreshVar 5;
  ]

let constructors = [ (fun x y -> Fun (x, y)); (fun x y -> Pair (x, y)) ]

let combine l a =
  l
  @ List.flatten
      (List.map
         (fun x ->
           List.flatten (List.map (fun y -> List.map (fun z -> z x y) a) l))
         l)

let combinedTypes1 = combine basicTypes constructors

let combinedTypes2 =
  List.map (fun x -> Mono x) (combine combinedTypes1 constructors)

let compareSubstitutions s1 s2 =
  List.for_all
    (fun x -> applySubstToTypeGenre s1 x = applySubstToTypeGenre s2 x)
    combinedTypes2

let unifyHelper t1 t2 = snd (State.IntState.runState (unifyOne t1 t2) ~init:0)

let unifyIntWithInt _ =
  (* ARRANGE *)
  let expected = emptySubst in
  (* ACT *)
  let output = unifyHelper (Mono Int) (Mono Int) in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyIntWithBool _ =
  (* ARRANGE *)
  let expected = UnifyException "Cannot unify int and bool" in
  (* ACT *)
  let output _ = unifyHelper (Mono Int) (Mono Bool) in
  (* ASSERT *)
  assert_raises expected output

let unifyVar0WithVar1 _ =
  (* ARRANGE *)
  let expected = substFromList [ Mono (FreshVar 1) ] in
  (* ACT *)
  let output = unifyHelper (Mono (FreshVar 0)) (Mono (FreshVar 1)) in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyFunVar0BoolWithFunIntVar1 _ =
  (* ARRANGE *)
  let expected = substFromList [ Mono Int; Mono Bool ] in
  (* ACT *)
  let output =
    unifyHelper (Mono (Fun (FreshVar 0, Bool))) (Mono (Fun (Int, FreshVar 1)))
  in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyPairVar2BoolWithPairIntVar1 _ =
  (* ARRANGE *)
  let expected = substFromList [ Mono (FreshVar 0); Mono Bool; Mono Int ] in
  (* ACT *)
  let output =
    unifyHelper (Mono (Pair (FreshVar 2, Bool))) (Mono (Pair (Int, FreshVar 1)))
  in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyPairVar2BoolWithVar0 _ =
  (* ARRANGE *)
  let expected = substFromList [ Mono (Pair (FreshVar 2, Bool)) ] in
  (* ACT *)
  let output =
    unifyHelper (Mono (Pair (FreshVar 2, Bool))) (Mono (FreshVar 0))
  in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyFunFunVar0Var0Var0WithFunVar1Int _ =
  (* ARRANGE *)
  let expected = substFromList [ Mono Int; Mono (Fun (Int, Int)) ] in
  (* ACT *)
  let output =
    unifyHelper
      (Mono (Fun (Fun (FreshVar 0, FreshVar 0), FreshVar 0)))
      (Mono (Fun (FreshVar 1, Int)))
  in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyForAllInt _ =
  (* ARRANGE *)
  let expected = substFromList [ Mono Int ] in
  (* ACT *)
  let output = unifyHelper (Poly (1, RhoMono (Var 0))) (Poly (1, RhoMono Int)) in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let suite =
  "UnifyTest"
  >::: [
         "unifyIntWithInt" >:: unifyIntWithInt;
         "unifyIntWithBool" >:: unifyIntWithBool;
         "unifyVar0WithVar1" >:: unifyVar0WithVar1;
         "unifyFunVar0BoolWithFunIntVar1" >:: unifyFunVar0BoolWithFunIntVar1;
         "unifyPairVar2BoolWithPairIntVar1" >:: unifyPairVar2BoolWithPairIntVar1;
         "unifyPairVar2BoolWithVar0" >:: unifyPairVar2BoolWithVar0;
         "unifyFunFunVar0Var0Var0WithFunVar1Int"
         >:: unifyFunFunVar0Var0Var0WithFunVar1Int;
         "unifyForAllInt" >:: unifyForAllInt;
       ]

let () = run_test_tt_main suite
