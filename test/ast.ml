open Sifun
open Utils
open Ast
open OUnit2

let intToString _ =
  (* ARRANGE *)
  let input = Int 1 in
  let expected = "Int(1)" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let varToString _ =
  (* ARRANGE *)
  let input = Var "a" in
  let expected = "Var(a)" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let boolToString _ =
  (* ARRANGE *)
  let input = Bool true in
  let expected = "Bool(true)" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let unitToString _ =
  (* ARRANGE *)
  let input = Unit in
  let expected = "Unit" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let pairToString _ =
  (* ARRANGE *)
  let input = Pair (Int 2, Bool false) in
  let expected = "Pair(Int(2),Bool(false))" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let firstToString _ =
  (* ARRANGE *)
  let input = Fst (Int 1) in
  let expected = "Fst(Int(1))" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let secondToString _ =
  (* ARRANGE *)
  let input = Snd Unit in
  let expected = "Snd(Unit)" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let funToString _ =
  (* ARRANGE *)
  let input = Fun ("x", Var "x") in
  let expected = "Fun(x,Var(x))" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let funTypeToString _ =
  (* ARRANGE *)
  let input = FunType ("x", Int, Var "y") in
  let expected = "Fun(x,Int,Var(y))" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let appToString _ =
  (* ARRANGE *)
  let input = App (Var "z", Var "y") in
  let expected = "App(Var(z),Var(y))" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let typeAppToString _ =
  (* ARRANGE *)
  let input = TypeApp (Var "z", Bool) in
  let expected = "TypeApp(Var(z),Bool)" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let lamToString _ =
  (* ARRANGE *)
  let input = Lam ("a", Pair (Unit, Unit)) in
  let expected = "Lam(a,Pair(Unit,Unit))" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let annotToString _ =
  (* ARRANGE *)
  let input = Annot (Pair (Unit, Unit), Pair (Int, Int)) in
  let expected = "Annot(Pair(Unit,Unit),Pair(Int,Int))" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let listToString _ =
  (* ARRANGE *)
  let input = List [ Int 1; Unit; Var "a" ] in
  let expected = "List(Int(1)\nUnit\nVar(a)\n)" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let suiteToString =
  "ASTToStringTests"
  >::: [
         "intToString" >:: intToString;
         "varToString" >:: varToString;
         "boolToString" >:: boolToString;
         "unitToString" >:: unitToString;
         "pairToString" >:: pairToString;
         "firstToString" >:: firstToString;
         "secondToString" >:: secondToString;
         "funToString" >:: funToString;
         "funTypeToString" >:: funTypeToString;
         "appToString" >:: appToString;
         "typeAppToString" >:: typeAppToString;
         "lamToString" >:: lamToString;
         "annotToString" >:: annotToString;
         "listToString" >:: listToString;
       ]

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

let suite =
  "LexerAndParserTests"
  >::: [
         "parseFunction" >:: parseFunction;
         "parseInt1" >:: parseInt1;
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
       ]

let () = run_test_tt_main suite
let () = run_test_tt_main suiteToString
