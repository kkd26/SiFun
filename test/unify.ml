open Sifun
open Type
open Unify
open OUnit2

let basicTypes = [Int; Bool; Unit; Var 0; Var 1; Var 2; Var 3; Var 4; Var 5]
let constructors = [(fun x y -> Fun (x, y)); (fun x y -> Pair (x, y))]

let combine l a =
  l
  @ List.flatten
      (List.map
         (fun x ->
           List.flatten (List.map (fun y -> List.map (fun z -> z x y) a) l) )
         l )

let combinedTypes1 = combine basicTypes constructors
let combinedTypes2 = combine combinedTypes1 constructors

let compareSubstitutions s1 s2 =
  List.for_all
    (fun x -> applySubstToMonoType s1 x = applySubstToMonoType s2 x)
    combinedTypes2

let unifyIntWithInt _ =
  (* ARRANGE *)
  let expected = emptySubst in
  (* ACT *)
  let output = unifyOne Int Int in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyIntWithBool _ =
  (* ARRANGE *)
  let expected = UnifyException "Cannot unify int and bool" in
  (* ACT *)
  let output _ = unifyOne Int Bool in
  (* ASSERT *)
  assert_raises expected output

let unifyVar0WithVar1 _ =
  (* ARRANGE *)
  let expected = substFromList [Var 1] in
  (* ACT *)
  let output = unifyOne (Var 0) (Var 1) in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyFunVar0BoolWithFunIntVar1 _ =
  (* ARRANGE *)
  let expected = substFromList [Int; Bool] in
  (* ACT *)
  let output = unifyOne (Fun (Var 0, Bool)) (Fun (Int, Var 1)) in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyPairVar2BoolWithPairIntVar1 _ =
  (* ARRANGE *)
  let expected = substFromList [Var 0; Bool; Int] in
  (* ACT *)
  let output = unifyOne (Pair (Var 2, Bool)) (Pair (Int, Var 1)) in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyPairVar2BoolWithVar0 _ =
  (* ARRANGE *)
  let expected = substFromList [Pair (Var 2, Bool)] in
  (* ACT *)
  let output = unifyOne (Pair (Var 2, Bool)) (Var 0) in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyFunFunVar0Var0Var0WithFunVar1Int _ =
  (* ARRANGE *)
  let expected = substFromList [Int; Fun (Int, Int)] in
  (* ACT *)
  let output = unifyOne (Fun (Fun (Var 0, Var 0), Var 0)) (Fun (Var 1, Int)) in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let suite =
  "UnifyTest"
  >::: [ "unifyIntWithInt" >:: unifyIntWithInt
       ; "unifyIntWithBool" >:: unifyIntWithBool
       ; "unifyVar0WithVar1" >:: unifyVar0WithVar1
       ; "unifyFunVar0BoolWithFunIntVar1" >:: unifyFunVar0BoolWithFunIntVar1
       ; "unifyPairVar2BoolWithPairIntVar1" >:: unifyPairVar2BoolWithPairIntVar1
       ; "unifyPairVar2BoolWithVar0" >:: unifyPairVar2BoolWithVar0
       ; "unifyFunFunVar0Var0Var0WithFunVar1Int"
         >:: unifyFunFunVar0Var0Var0WithFunVar1Int ]

let () = run_test_tt_main suite
