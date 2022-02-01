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
let combinedTypes2 = combine combinedTypes1 constructors

let compareSubstitutions s1 s2 =
  List.for_all
    (fun x -> applySubstToMonoType s1 x = applySubstToMonoType s2 x)
    combinedTypes2

let unifyHelper t1 t2 = snd (State.IntState.runState (unifyOne t1 t2) ~init:0)

let unifyIntWithInt _ =
  (* ARRANGE *)
  let expected = emptySubst in
  (* ACT *)
  let output = unifyHelper Int Int in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyIntWithBool _ =
  (* ARRANGE *)
  let expected = UnifyException "Cannot unify int and bool" in
  (* ACT *)
  let output _ = unifyHelper Int Bool in
  (* ASSERT *)
  assert_raises expected output

let unifyVar0WithVar1 _ =
  (* ARRANGE *)
  let expected = substFromList [ FreshVar 1 ] in
  (* ACT *)
  let output = unifyHelper (FreshVar 0) (FreshVar 1) in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyFunVar0BoolWithFunIntVar1 _ =
  (* ARRANGE *)
  let expected = substFromList [ Int; Bool ] in
  (* ACT *)
  let output = unifyHelper (Fun (FreshVar 0, Bool)) (Fun (Int, FreshVar 1)) in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyPairVar2BoolWithPairIntVar1 _ =
  (* ARRANGE *)
  let expected = substFromList [ FreshVar 0; Bool; Int ] in
  (* ACT *)
  let output = unifyHelper (Pair (FreshVar 2, Bool)) (Pair (Int, FreshVar 1)) in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyPairVar2BoolWithVar0 _ =
  (* ARRANGE *)
  let expected = substFromList [ Pair (FreshVar 2, Bool) ] in
  (* ACT *)
  let output = unifyHelper (Pair (FreshVar 2, Bool)) (FreshVar 0) in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyFunFunVar0Var0Var0WithFunVar1Int _ =
  (* ARRANGE *)
  let expected = substFromList [ Int; Fun (Int, Int) ] in
  (* ACT *)
  let output =
    unifyHelper
      (Fun (Fun (FreshVar 0, FreshVar 0), FreshVar 0))
      (Fun (FreshVar 1, Int))
  in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyForAllInt _ =
  (* ARRANGE *)
  let expected = substFromList [ Int ] in
  (* ACT *)
  let output = unifyHelper (ForAll (Var 0)) (ForAll Int) in
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
