open Sifun
open Utils
open Ast
open OUnit2

let parseUnit _ =
  (* ARRANGE *)
  let input = "()" in
  let expected = [ Unit ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseBool _ =
  (* ARRANGE *)
  let input = "true" in
  let expected = [ Bool true ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseInt1 _ =
  (* ARRANGE *)
  let input = "1" in
  let expected = [ Int 1 ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseInt1Int2 _ =
  (* ARRANGE *)
  let input = "1;2" in
  let expected = [ Int 1; Int 2 ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parsePairInt1Int2 _ =
  (* ARRANGE *)
  let input = "(1,2)" in
  let expected = [ Pair (Int 1, Int 2) ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parsePairUnitBoolFalse _ =
  (* ARRANGE *)
  let input = "((),false)" in
  let expected = [ Pair (Unit, Bool false) ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseFunction _ =
  (* ARRANGE *)
  let input = "fn x => x" in
  let expected = [ Fun ("x", Var "x") ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseFirst _ =
  (* ARRANGE *)
  let input = "fst(1,2)" in
  let expected = [ Fst (Pair (Int 1, Int 2)) ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseSecond _ =
  (* ARRANGE *)
  let input = "snd(true,false)" in
  let expected = [ Snd (Pair (Bool true, Bool false)) ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseNestedFunctions _ =
  (* ARRANGE *)
  let input = "fn x => fn x => fn x => x" in
  let expected = [ Fun ("x", Fun ("x", Fun ("x", Var "x"))) ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseNestedFunctionsWithApplication _ =
  (* ARRANGE *)
  let input = "fn x => fn x => fn x => x y" in
  let expected = [ Fun ("x", Fun ("x", Fun ("x", App (Var "x", Var "y")))) ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseNestedFunctionsWithApplication2 _ =
  (* ARRANGE *)
  let input = "fn x => fn x => (fn x => x) y" in
  let expected = [ Fun ("x", Fun ("x", App (Fun ("x", Var "x"), Var "y"))) ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseApplication _ =
  (* ARRANGE *)
  let input = "x y z" in
  let expected = [ App (App (Var "x", Var "y"), Var "z") ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let parseExprInParenthesis _ =
  (* ARRANGE *)
  let input = "(1 2)" in
  let expected = [ App (Int 1, Int 2) ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let firstAndApplication _ =
  (* ARRANGE *)
  let input = "fst x y" in
  let expected = [ App (Fst (Var "x"), Var "y") ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let firstAndTypeApplication _ =
  (* ARRANGE *)
  let input = "fst x {int}" in
  let expected = [ TypeApp (Fst (Var "x"), Int) ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let firstAndTypeApplication2 _ =
  (* ARRANGE *)
  let input = "fst x {int} {bool}" in
  let expected = [ TypeApp (TypeApp (Fst (Var "x"), Int), Bool) ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let typedFunction _ =
  (* ARRANGE *)
  let input = "fn x : int => x" in
  let expected = [ FunType ("x", Int, Var "x") ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let lambdaTypeAbstraction _ =
  (* ARRANGE *)
  let input = "lam a. fn x : a => x" in
  let expected = [ Lam ("a", FunType ("x", Var "a", Var "x")) ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let nestedLambdaTypeAbstraction _ =
  (* ARRANGE *)
  let input = "lam a b c. fn x : b => fn y => fn z : a => 1" in
  let expected =
    [
      Lam
        ( "a",
          Lam
            ( "b",
              Lam
                ( "c",
                  FunType
                    ("x", Var "b", Fun ("y", FunType ("z", Var "a", Int 1))) )
            ) );
    ]
  in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let lambdaTypeWithForAll _ =
  (* ARRANGE *)
  let input = "lam a. fn x : forall a b c. a -> b => x" in
  let expected =
    [
      Lam
        ( "a",
          FunType
            ( "x",
              ForAll ("a", ForAll ("b", ForAll ("c", Fun (Var "a", Var "b")))),
              Var "x" ) );
    ]
  in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let nestedLambda _ =
  (* ARRANGE *)
  let input = "lam a b c. fn x : a => lam a b c d. fn y : a => x" in
  let expected =
    [
      Lam
        ( "a",
          Lam
            ( "b",
              Lam
                ( "c",
                  FunType
                    ( "x",
                      Var "a",
                      Lam
                        ( "a",
                          Lam
                            ( "b",
                              Lam
                                ("c", Lam ("d", FunType ("y", Var "a", Var "x")))
                            ) ) ) ) ) );
    ]
  in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let list _ =
  (* ARRANGE *)
  let input = "[1;2;3]" in
  let expected = [ List [ Int 1; Int 2; Int 3 ] ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let newline _ =
  (* ARRANGE *)
  let input = "1\n2" in
  let expected = [ App (Int 1, Int 2) ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let syntaxErrorUnexpectedCharacter _ =
  (* ARRANGE *)
  let input = "1+2" in
  let expected =
    LexBufException "File \"\", line 1, character 2:\nUnexpected char: +"
  in
  (* ACT *)
  let output _ = stringToExprList input in
  (* ASSERT *)
  assert_raises expected output

let blockSemicolonFront _ =
  (* ARRANGE *)
  let input = ";1" in
  let expected = [ Int 1 ] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let blockEmpty _ =
  (* ARRANGE *)
  let input = "" in
  let expected = [] in
  (* ACT *)
  let output = stringToExprList input in
  (* ASSERT *)
  assert_equal expected output

let suite =
  "LexerAndParserTests"
  >::: [
         "parseFunction" >:: parseFunction;
         "parseInt1" >:: parseInt1;
         "parseInt1Int2" >:: parseInt1Int2;
         "parsePairInt1Int2\n      " >:: parsePairInt1Int2;
         "parseFirst" >:: parseFirst;
         "parseNestedFunctions" >:: parseNestedFunctions;
         "parseNestedFunctionsWithApplication"
         >:: parseNestedFunctionsWithApplication;
         "parseNestedFunctionsWithApplication2"
         >:: parseNestedFunctionsWithApplication2;
         "parseApplication" >:: parseApplication;
         "parseUnit" >:: parseUnit;
         "parseBool" >:: parseBool;
         "parsePairUnitBoolFalse" >:: parsePairUnitBoolFalse;
         "parseSecond" >:: parseSecond;
         "parseExprInParenthesis" >:: parseExprInParenthesis;
         "firstAndApplication" >:: firstAndApplication;
         "firstAndTypeApplication" >:: firstAndTypeApplication;
         "firstAndTypeApplication2" >:: firstAndTypeApplication2;
         "typedFunction" >:: typedFunction;
         "lambdaTypeAbstraction" >:: lambdaTypeAbstraction;
         "nestedLambdaTypeAbstraction" >:: nestedLambdaTypeAbstraction;
         "lambdaTypeWithForAll" >:: lambdaTypeWithForAll;
         "nestedLambda" >:: nestedLambda;
         "list" >:: list;
         "newline" >:: newline;
         "syntaxErrorUnexpectedCharacter" >:: syntaxErrorUnexpectedCharacter;
         "blockSemicolonFront" >:: blockSemicolonFront;
         "blockEmpty" >:: blockEmpty;
       ]

let () = run_test_tt_main suite
