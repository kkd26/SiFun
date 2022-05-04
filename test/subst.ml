open Sifun
open OUnit2
open DBType
open Subst

let substFreshVar0ForInt _ =
  (* ARRANGE *)
  let input = FreshVar 0 in
  let x = 0 in
  let tk = Mono Int in
  let expected = Mono Int in
  (* ACT *)
  let output = substituteMono tk x input in
  (* ASSERT *)
  assert_equal expected output

let substListFreshVar0ForInt _ =
  (* ARRANGE *)
  let input = List (FreshVar 0) in
  let x = 0 in
  let tk = Mono Int in
  let expected = Rho (RhoList (0, RhoMono Int)) in
  (* ACT *)
  let output = substituteMono tk x input in
  (* ASSERT *)
  assert_equal expected output

let substRhoFunFreshVar0ForInt _ =
  (* ARRANGE *)
  let input = RhoFun ((0, RhoMono (FreshVar 0)), (0, RhoMono (FreshVar 1))) in
  let x = 0 in
  let tk = Mono Int in
  let expected = Rho (RhoFun ((0, RhoMono Int), (0, RhoMono (FreshVar 1)))) in
  (* ACT *)
  let output = substituteRho tk x input in
  (* ASSERT *)
  assert_equal expected output

let substRhoListFreshVar0ForInt _ =
  (* ARRANGE *)
  let input = RhoList (0, RhoMono (FreshVar 0)) in
  let x = 0 in
  let tk = Mono Int in
  let expected = Rho (RhoList (0, RhoMono Int)) in
  (* ACT *)
  let output = substituteRho tk x input in
  (* ASSERT *)
  assert_equal expected output

let emptySubstToStringTest _ =
  (* ARRANGE *)
  let input = [] in
  let expected = "e " in
  (* ACT *)
  let output = substToString input in
  (* ASSERT *)
  assert_equal expected output

let intBoolSubstToStringTest _ =
  (* ARRANGE *)
  let input = [ (0, Mono Int); (1, Mono Bool) ] in
  let expected = "e 0:(int) 1:(bool) " in
  (* ACT *)
  let output = substToString input in
  (* ASSERT *)
  assert_equal expected output

let main =
  "SubstituteTests"
  >::: [
         "substFreshVar0ForInt" >:: substFreshVar0ForInt;
         "substListFreshVar0ForInt" >:: substListFreshVar0ForInt;
         "substRhoFunFreshVar0ForInt" >:: substRhoFunFreshVar0ForInt;
         "substRhoListFreshVar0ForInt" >:: substRhoListFreshVar0ForInt;
         "emptySubstToStringTest" >:: emptySubstToStringTest;
         "intBoolSubstToStringTest" >:: intBoolSubstToStringTest;
       ]

let () = run_test_tt_main main
