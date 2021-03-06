open Sifun
open Prenex
open DBType
open OUnit2

let prenexInt _ =
  (* ARRANGE *)
  let input = Mono Int in
  let expected = (0, RhoMono Int) in
  (* ACT *)
  let output = pr input in
  (* ASSERT *)
  assert_equal output expected

let prenexBool _ =
  (* ARRANGE *)
  let input = Mono Bool in
  let expected = (0, RhoMono Bool) in
  (* ACT *)
  let output = pr input in
  (* ASSERT *)
  assert_equal output expected

let prenexUnit _ =
  (* ARRANGE *)
  let input = Mono Unit in
  let expected = (0, RhoMono Unit) in
  (* ACT *)
  let output = pr input in
  (* ASSERT *)
  assert_equal output expected

let prenexPair _ =
  (* ARRANGE *)
  let input = Mono (Pair (Int, Bool)) in
  let expected = (0, RhoMono (Pair (Int, Bool))) in
  (* ACT *)
  let output = pr input in
  (* ASSERT *)
  assert_equal output expected

let prenexFun _ =
  (* ARRANGE *)
  let input = Mono (Fun (Int, Bool)) in
  let expected = (0, RhoMono (Fun (Int, Bool))) in
  (* ACT *)
  let output = pr input in
  (* ASSERT *)
  assert_equal output expected

let prenexFunNotNormalized _ =
  (* ARRANGE *)
  let input = Mono (Fun (Int, Bool)) in
  let expected = (0, RhoFun ((0, RhoMono Int), (0, RhoMono Bool))) in
  let expectedNorm = typeGenreToPoly (normalize (Poly expected)) in
  let expectedEx = OUnitTest.OUnit_failure "not equal" in
  (* ACT *)
  let output = pr input in
  (* ASSERT *)
  assert_equal output expectedNorm;
  assert_raises expectedEx (fun _ -> assert_equal output expected)

let polyOnlyBoundedVariables _ =
  (* ARRANGE *)
  let input =
    Poly (2, RhoFun ((1, RhoMono (Var 0)), (1, RhoMono (Fun (Var 1, Var 0)))))
  in
  let expected =
    (3, RhoFun ((1, RhoMono (Var 0)), (0, RhoMono (Fun (Var 1, Var 0)))))
  in
  (* ACT *)
  let output = pr input in
  (* ASSERT *)
  assert_equal output expected

let polyMonoTypeFun _ =
  (* ARRANGE *)
  let input = Poly (2, RhoMono (Fun (Var 1, Fun (Var 0, Var 0)))) in
  let expected = (2, RhoMono (Fun (Var 1, Fun (Var 0, Var 0)))) in
  (* ACT *)
  let output = pr input in
  (* ASSERT *)
  assert_equal output expected

let polyOneUnboundedVariable _ =
  (* ARRANGE *)
  let input =
    Poly (2, RhoFun ((1, RhoMono (Var 1)), (1, RhoMono (Fun (Var 1, Var 0)))))
  in
  let expected =
    (3, RhoFun ((1, RhoMono (Var 2)), (0, RhoMono (Fun (Var 1, Var 0)))))
  in
  (* ACT *)
  let output = pr input in
  (* ASSERT *)
  assert_equal output expected

let polyNestedFunctions _ =
  (* ARRANGE *)
  let input =
    Poly (1, RhoFun ((0, RhoMono (Var 0)), (1, RhoMono (Fun (Var 0, Var 0)))))
  in
  let expected = (2, RhoMono (Fun (Var 1, Fun (Var 0, Var 0)))) in
  (* ACT *)
  let output = pr input in
  (* ASSERT *)
  assert_equal output expected

let rhoPair _ =
  (* ARRANGE *)
  let input =
    Poly (1, RhoPair ((3, RhoMono (Var 2)), (1, RhoMono (Pair (Var 0, Var 0)))))
  in
  let expected =
    (2, RhoPair ((3, RhoMono (Var 2)), (0, RhoMono (Pair (Var 0, Var 0)))))
  in
  (* ACT *)
  let output = pr input in
  (* ASSERT *)
  assert_equal output expected

let rhoPair2 _ =
  (* ARRANGE *)
  let input =
    Poly (1, RhoPair ((3, RhoMono (Var 3)), (1, RhoMono (Pair (Var 0, Var 0)))))
  in
  let expected =
    (2, RhoPair ((3, RhoMono (Var 4)), (0, RhoMono (Pair (Var 0, Var 0)))))
  in
  (* ACT *)
  let output = pr input in
  (* ASSERT *)
  assert_equal output expected

let rhoPair3 _ =
  (* ARRANGE *)
  let input =
    Poly (1, RhoPair ((3, RhoMono (Var 3)), (10, RhoMono (Pair (Var 0, Var 0)))))
  in
  let expected =
    (11, RhoPair ((3, RhoMono (Var 13)), (0, RhoMono (Pair (Var 0, Var 0)))))
  in
  (* ACT *)
  let output = pr input in
  (* ASSERT *)
  assert_equal output expected

let monoException _ =
  (* ARRANGE *)
  let input = Poly (0, RhoMono Int) in
  let expected = (0, RhoMono Int) in
  (* ACT *)
  let output = pr input in
  (* ASSERT *)
  assert_equal output expected

let polyList _ =
  (* ARRANGE *)
  let input = Rho (RhoList (1, RhoMono Int)) in
  let expected = (0,RhoList (1, RhoMono Int)) in
  (* ACT *)
  let output = pr input in
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
         "rhoPair" >:: rhoPair;
         "rhoPair2" >:: rhoPair2;
         "rhoPair3" >:: rhoPair3;
         "monoException" >:: monoException;
         "polyList" >:: polyList;
       ]

let () = run_test_tt_main suite
