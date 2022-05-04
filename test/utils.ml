open Sifun
open OUnit2
open Utils

let combineFunctions _ =
  (* ARRANGE *)
  let input = 3 in
  let input1 x = x + 1 in
  let input2 x = x * 2 in
  let expected = 7 in
  (* ACT *)
  let output = (input1 $ input2) input in
  (* ASSERT *)
  assert_equal expected output

let printAst _ =
  (* ARRANGE *)
  let input = [ Ast.Int 1 ] in
  let expected = () in
  (* ACT *)
  let output = printAst input in
  (* ASSERT *)
  assert_equal expected output

let printDBAst _ =
  (* ARRANGE *)
  let input = [ DBAst.Int 1 ] in
  let expected = () in
  (* ACT *)
  let output = printDBAst input in
  (* ASSERT *)
  assert_equal expected output

let printTypeExpr _ =
  (* ARRANGE *)
  let input = DBType.Int in
  let expected = () in
  (* ACT *)
  let output = printTypeExpr input in
  (* ASSERT *)
  assert_equal expected output

let printTypeGenre _ =
  (* ARRANGE *)
  let input = DBType.Mono Int in
  let expected = () in
  (* ACT *)
  let output = printTypeGenre input in
  (* ASSERT *)
  assert_equal expected output

let printSubst _ =
  (* ARRANGE *)
  let input = [] in
  let expected = () in
  (* ACT *)
  let output = printSubst input in
  (* ASSERT *)
  assert_equal expected output

let printCtx _ =
  (* ARRANGE *)
  let input = [ DBType.Mono Int ] in
  let expected = () in
  (* ACT *)
  let output = printCtx input in
  (* ASSERT *)
  assert_equal expected output

let printTypeAndTypeGenre _ =
  (* ARRANGE *)
  let input = (DBType.Mono Int, DBAst.Int 1) in
  let expected = () in
  (* ACT *)
  let output = printTypeAndTypeGenre input in
  (* ASSERT *)
  assert_equal expected output

let main =
  "UtilsTests"
  >::: [
         "combineFunctions" >:: combineFunctions;
         "printAst" >:: printAst;
         "printDBAst" >:: printDBAst;
         "printTypeExpr" >:: printTypeExpr;
         "printTypeGenre" >:: printTypeGenre;
         "printSubst" >:: printSubst;
         "printCtx" >:: printCtx;
         "printTypeAndTypeGenre" >:: printTypeAndTypeGenre;
       ]

let () = run_test_tt_main main
