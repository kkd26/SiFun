open Sifun
open Type
open Subst
open Unify
open OUnit2

let basicTypes =
  [Int; Bool; Unit; Var "a"; Var "b"; Var "c"; Var "d"; Var "e"; Var "f"]

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
  let expected = substFromList [Var "b"] in
  (* ACT *)
  let output = unifyOne (Var "a") (Var "b") in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyFunVar0BoolWithFunIntVar1 _ =
  (* ARRANGE *)
  let expected = substFromList [Int; Bool] in
  (* ACT *)
  let output = unifyOne (Fun (Var "a", Bool)) (Fun (Int, Var "b")) in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyPairVar2BoolWithPairIntVar1 _ =
  (* ARRANGE *)
  let expected = substFromList [Var "a"; Bool; Int] in
  (* ACT *)
  let output = unifyOne (Pair (Var "c", Bool)) (Pair (Int, Var "b")) in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyPairVar2BoolWithVar0 _ =
  (* ARRANGE *)
  let expected = substFromList [Pair (Var "c", Bool)] in
  (* ACT *)
  let output = unifyOne (Pair (Var "c", Bool)) (Var "a") in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyFunFunVar0Var0Var0WithFunVar1Int _ =
  (* ARRANGE *)
  let expected = substFromList [Int; Fun (Int, Int)] in
  (* ACT *)
  let output =
    unifyOne (Fun (Fun (Var "a", Var "a"), Var "a")) (Fun (Var "b", Int)) in
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
