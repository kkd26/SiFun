open Sifun
open Prenex
open DBType
open OUnit2

let prenexInt _ =
  (* ARRANGE *)
  let input = Mono Int in
  let expected = (0, T Int) in
  (* ACT *)
  let output = pr input in
  (* ASSERT *)
  assert_equal output expected

let prenexBool _ =
  (* ARRANGE *)
  let input = Mono Bool in
  let expected = (0, T Bool) in
  (* ACT *)
  let output = pr input in
  (* ASSERT *)
  assert_equal output expected

let prenexUnit _ =
  (* ARRANGE *)
  let input = Mono Unit in
  let expected = (0, T Unit) in
  (* ACT *)
  let output = pr input in
  (* ASSERT *)
  assert_equal output expected

let prenexPair _ =
  (* ARRANGE *)
  let input = Mono (Pair (Int, Bool)) in
  let expected = (0, T (Pair (Int, Bool))) in
  (* ACT *)
  let output = pr input in
  (* ASSERT *)
  assert_equal output expected

let prenexFun _ =
  (* ARRANGE *)
  let input = Mono (Fun (Int, Bool)) in
  let expected = (0, T (Fun (Int, Bool))) in
  (* ACT *)
  let output = pr input in
  (* ASSERT *)
  assert_equal output expected

let prenexFunNotNormalized _ =
  (* ARRANGE *)
  let input = Mono (Fun (Int, Bool)) in
  let expected = (0, F ((0, T Int), (0, T Bool))) in
  let expectedNorm = typeKindToPoly (normalize (Poly expected)) in
  let expectedEx = OUnitTest.OUnit_failure "not equal" in
  (* ACT *)
  let output = pr input in
  (* ASSERT *)
  assert_equal output expectedNorm;
  assert_raises expectedEx (fun _ -> assert_equal output expected)

let polyOnlyBoundedVariables _ =
  (* ARRANGE *)
  let input = Poly (2, F ((1, T (Var 0)), (1, T (Fun (Var 1, Var 0))))) in
  let expected = (3, F ((1, T (Var 0)), (0, T (Fun (Var 1, Var 0))))) in
  (* ACT *)
  let output = pr input in
  (* ASSERT *)
  assert_equal output expected

let polyMonoTypeFun _ =
  (* ARRANGE *)
  let input = Poly (2, T (Fun (Var 1, Fun (Var 0, Var 0)))) in
  let expected = (2, T (Fun (Var 1, Fun (Var 0, Var 0)))) in
  (* ACT *)
  let output = pr input in
  (* ASSERT *)
  assert_equal output expected

let polyOneUnboundedVariable _ =
  (* ARRANGE *)
  let input = Poly (2, F ((1, T (Var 1)), (1, T (Fun (Var 1, Var 0))))) in
  let expected = (3, F ((1, T (Var 2)), (0, T (Fun (Var 1, Var 0))))) in
  (* ACT *)
  let output = pr input in
  (* ASSERT *)
  assert_equal output expected

let polyNestedFunctions _ =
  (* ARRANGE *)
  let input = Poly (1, F ((0, T (Var 0)), (1, T (Fun (Var 0, Var 0))))) in
  let expected = (2, T (Fun (Var 1, Fun (Var 0, Var 0)))) in
  Utils.printTypeKind input;
  Utils.printTypeKind (Poly expected);
  (* ACT *)
  let output = pr input in
  Utils.printTypeKind (Poly output);
  (* ASSERT *)
  assert_equal output expected

let suite =
  "PrenexTest"
  >::: [
         "prenexInt" >:: prenexInt;
         "prenexBool" >:: prenexBool;
         "prenexUnit" >:: prenexUnit;
         "prenexPair" >:: prenexPair;
         "prenexFun" >:: prenexFun;
         "prenexFunNotNormalized" >:: prenexFunNotNormalized;
         "polyOnlyBoundedVariables" >:: polyOnlyBoundedVariables;
         "polyMonoTypeFun" >:: polyMonoTypeFun;
         "polyOneUnboundedVariable" >:: polyOneUnboundedVariable;
         "polyNestedFunctions" >:: polyNestedFunctions;
       ]

let () = run_test_tt_main suite
