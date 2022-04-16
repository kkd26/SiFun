open Sifun
open Utils
open Ast
open OUnit2

let parseUnit _ =
  (* ARRANGE *)
  let input = "(): unit" in
  let expected = [ Annot (Unit, Unit) ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseInt _ =
  (* ARRANGE *)
  let input = "1: int" in
  let expected = [ Annot (Int 1, Int) ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseBool _ =
  (* ARRANGE *)
  let input = "false: bool" in
  let expected = [ Annot (Bool false, Bool) ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseVar _ =
  (* ARRANGE *)
  let input = "2: a" in
  let expected = [ Annot (Int 2, Var "a") ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseIntParenthesis _ =
  (* ARRANGE *)
  let input = "2: (int)" in
  let expected = [ Annot (Int 2, Int) ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parsePair _ =
  (* ARRANGE *)
  let input = "(1, false): (int, bool)" in
  let expected = [ Annot (Pair (Int 1, Bool false), Pair (Int, Bool)) ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseFun _ =
  (* ARRANGE *)
  let input = "(fn x : int => true): (int -> bool)" in
  let expected = [ Annot (FunType ("x", Int, Bool true), Fun (Int, Bool)) ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseDoubleFun _ =
  (* ARRANGE *)
  let input = "(fn x : int => fn y : bool => 5): (int -> bool -> int)" in
  let expected =
    [
      Annot
        ( FunType ("x", Int, FunType ("y", Bool, Int 5)),
          Fun (Int, Fun (Bool, Int)) );
    ]
  in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseDoubleHigherFun _ =
  (* ARRANGE *)
  let input = "8 {((int -> bool) -> int)}" in
  let expected = [ TypeApp (Int 8, Fun (Fun (Int, Bool), Int)) ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseFunForall _ =
  (* ARRANGE *)
  let input = "8 {(int -> forall a. a->b)}" in
  let expected =
    [ TypeApp (Int 8, Fun (Int, ForAll ("a", Fun (Var "a", Var "b")))) ]
  in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseFunForall2 _ =
  (* ARRANGE *)
  let input = "8 {(int -> forall a. a)->b}" in
  let expected =
    [ TypeApp (Int 8, Fun (Fun (Int, ForAll ("a", Var "a")), Var "b")) ]
  in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseDoubleAnnot _ =
  (* ARRANGE *)
  let input = "(5 : int) : int" in
  let expected = [ Annot (Annot (Int 5, Int), Int) ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseDoubleTypeApp _ =
  (* ARRANGE *)
  let input = "1 {int} 2 {bool}" in
  let expected = [ App (TypeApp (Int 1, Int), TypeApp (Int 2, Bool)) ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let suite =
  "LexerAndParserTestsForTypes"
  >::: [
         "parseUnit" >:: parseUnit;
         "parseInt" >:: parseInt;
         "parseBool" >:: parseBool;
         "parseVar" >:: parseVar;
         "parseIntParenthesis" >:: parseIntParenthesis;
         "parsePair" >:: parsePair;
         "parseFun" >:: parseFun;
         "parseDoubleFun" >:: parseDoubleFun;
         "parseDoubleHigherFun" >:: parseDoubleHigherFun;
         "parseFunForall" >:: parseFunForall;
         "parseFunForall2" >:: parseFunForall2;
         "parseDoubleAnnot" >:: parseDoubleAnnot;
         "parseDoubleTypeApp" >:: parseDoubleTypeApp;
       ]

let () = run_test_tt_main suite
