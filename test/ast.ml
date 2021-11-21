open Sifun
open Utils
open Ast
open OUnit2

let parseFunction _ =
  (* ARRANGE *)
  let input = "fn x => x" in
  let expected = [Fun ("x", Var "x")] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseInt1 _ =
  (* ARRANGE *)
  let input = "1" in
  let expected = [Int 1] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parsePair _ =
  (* ARRANGE *)
  let input = "(1,2)" in
  let expected = [Pair (Int 1, Int 2)] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseFirst _ =
  (* ARRANGE *)
  let input = "fst(1,2)" in
  let expected = [Fst (Pair (Int 1, Int 2))] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseNestedFunctions _ =
  (* ARRANGE *)
  let input = "fn x => fn x => fn x => x" in
  let expected = [Fun ("x", Fun ("x", Fun ("x", Var "x")))] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseNestedFunctionsWithApplication _ =
  (* ARRANGE *)
  let input = "fn x => fn x => fn x => x y" in
  let expected = [Fun ("x", Fun ("x", Fun ("x", App (Var "x", Var "y"))))] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseNestedFunctionsWithApplication2 _ =
  (* ARRANGE *)
  let input = "fn x => fn x => (fn x => x) y" in
  let expected = [Fun ("x", Fun ("x", App (Fun ("x", Var "x"), Var "y")))] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseApplication _ =
  (* ARRANGE *)
  let input = "x y z" in
  let expected = [App (App (Var "x", Var "y"), Var "z")] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseUnit _ =
  (* ARRANGE *)
  let input = "()" in
  let expected = [Unit] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let suite =
  "LexerAndParserTests"
  >::: [ "parseFunction" >:: parseFunction; "parseInt1" >:: parseInt1
       ; "parsePair" >:: parsePair; "parseFirst" >:: parseFirst
       ; "parseNestedFunctions" >:: parseNestedFunctions
       ; "parseNestedFunctionsWithApplication"
         >:: parseNestedFunctionsWithApplication
       ; "parseNestedFunctionsWithApplication2"
         >:: parseNestedFunctionsWithApplication2
       ; "parseApplication" >:: parseApplication; "parseUnit" >:: parseUnit ]

let () = run_test_tt_main suite
