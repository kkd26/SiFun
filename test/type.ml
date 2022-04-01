open Sifun
open Type
open OUnit2

let intToString _ =
  (* ARRANGE *)
  let input = Int in
  let expected = "Int" in
  (* ACT *)
  let output = typeExprToString input in
  (* ASSERT *)
  assert_equal expected output

let varToString _ =
  (* ARRANGE *)
  let input = Var "a" in
  let expected = "Var(a)" in
  (* ACT *)
  let output = typeExprToString input in
  (* ASSERT *)
  assert_equal expected output

let boolToString _ =
  (* ARRANGE *)
  let input = Bool in
  let expected = "Bool" in
  (* ACT *)
  let output = typeExprToString input in
  (* ASSERT *)
  assert_equal expected output

let unitToString _ =
  (* ARRANGE *)
  let input = Unit in
  let expected = "Unit" in
  (* ACT *)
  let output = typeExprToString input in
  (* ASSERT *)
  assert_equal expected output

let pairToString _ =
  (* ARRANGE *)
  let input = Pair (Int, Bool) in
  let expected = "Pair(Int,Bool)" in
  (* ACT *)
  let output = typeExprToString input in
  (* ASSERT *)
  assert_equal expected output

let funToString _ =
  (* ARRANGE *)
  let input = Fun (Int, Int) in
  let expected = "Fun(Int,Int)" in
  (* ACT *)
  let output = typeExprToString input in
  (* ASSERT *)
  assert_equal expected output

let forAllToString _ =
  (* ARRANGE *)
  let input = ForAll ("x", Var "x") in
  let expected = "ForAll(x,Var(x))" in
  (* ACT *)
  let output = typeExprToString input in
  (* ASSERT *)
  assert_equal expected output

let listToString _ =
  (* ARRANGE *)
  let input = List (Pair (Int, Bool)) in
  let expected = "List(Pair(Int,Bool))" in
  (* ACT *)
  let output = typeExprToString input in
  (* ASSERT *)
  assert_equal expected output

let suiteToString =
  "TypeAstToStringTest"
  >::: [
         "intToString" >:: intToString;
         "varToString" >:: varToString;
         "boolToString" >:: boolToString;
         "unitToString" >:: unitToString;
         "pairToString" >:: pairToString;
         "funToString" >:: funToString;
         "forAllToString" >:: forAllToString;
         "listToString" >:: listToString;
       ]

let () = run_test_tt_main suiteToString
