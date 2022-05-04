open Sifun
open Ast
open DBAst
open OUnit2

let toDeBruijnUnit _ =
  (* ARRANGE *)
  let input : Ast.expr = Unit in
  let expected = Unit in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let toDeBruijnBool _ =
  (* ARRANGE *)
  let input : Ast.expr = Bool true in
  let expected = Bool true in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let toDeBruijnInt1 _ =
  (* ARRANGE *)
  let input : Ast.expr = Int 1 in
  let expected = Int 1 in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let toDeBruijnPairInt1Int2 _ =
  (* ARRANGE *)
  let input : Ast.expr = Pair (Int 1, Int 2) in
  let expected = Pair (Int 1, Int 2) in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let toDeBruijnPairUnitBoolFalse _ =
  (* ARRANGE *)
  let input : Ast.expr = Pair (Unit, Bool false) in
  let expected = Pair (Unit, Bool false) in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let toDeBruijnFunction _ =
  (* ARRANGE *)
  let input : Ast.expr = Fun ("x", Var "x") in
  let expected = Fun (Var 0) in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let toDeBruijnFirst _ =
  (* ARRANGE *)
  let input : Ast.expr = Fst (Pair (Int 1, Int 2)) in
  let expected = Fst (Pair (Int 1, Int 2)) in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let toDeBruijnSecond _ =
  (* ARRANGE *)
  let input : Ast.expr = Snd (Pair (Bool true, Bool false)) in
  let expected = Snd (Pair (Bool true, Bool false)) in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let toDeBruijnNestedFunctions _ =
  (* ARRANGE *)
  let input : Ast.expr = Fun ("x", Fun ("x", Fun ("x", Var "x"))) in
  let expected = Fun (Fun (Fun (Var 0))) in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let toDeBruijnNestedFunctionsWithApplication _ =
  (* ARRANGE *)
  let input : Ast.expr =
    Fun ("y", Fun ("x", Fun ("x", Fun ("x", App (Var "x", Var "y")))))
  in
  let expected = Fun (Fun (Fun (Fun (App (Var 0, Var 3))))) in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let toDeBruijnNestedFunctionsWithApplication2 _ =
  (* ARRANGE *)
  let input : Ast.expr =
    Fun ("y", Fun ("x", Fun ("x", App (Fun ("x", Var "x"), Var "y"))))
  in
  let expected = Fun (Fun (Fun (App (Fun (Var 0), Var 2)))) in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let toDeBruijnApplication _ =
  (* ARRANGE *)
  let input : Ast.expr =
    Fun ("x", Fun ("y", Fun ("z", App (App (Var "x", Var "y"), Var "z"))))
  in
  let expected = Fun (Fun (Fun (App (App (Var 2, Var 1), Var 0)))) in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let toDeBruijnExprInParenthesis _ =
  (* ARRANGE *)
  let input : Ast.expr = App (Int 1, Int 2) in
  let expected = App (Int 1, Int 2) in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let firstAndApplication _ =
  (* ARRANGE *)
  let input : Ast.expr = Fun ("x", Fun ("y", App (Fst (Var "x"), Var "y"))) in
  let expected = Fun (Fun (App (Fst (Var 1), Var 0))) in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let firstAndTypeApplication _ =
  (* ARRANGE *)
  let input : Ast.expr = Fun ("x", TypeApp (Fst (Var "x"), Int)) in
  let expected = Fun (TypeApp (Fst (Var 0), Mono Int)) in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let firstAndTypeApplication2 _ =
  (* ARRANGE *)
  let input : Ast.expr =
    Fun ("x", TypeApp (TypeApp (Fst (Var "x"), Int), Bool))
  in
  let expected = Fun (TypeApp (TypeApp (Fst (Var 0), Mono Int), Mono Bool)) in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let typedFunction _ =
  (* ARRANGE *)
  let input : Ast.expr = FunType ("x", Int, Var "x") in
  let expected = FunType (Mono Int, Var 0) in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let lambdaTypeAbstraction _ =
  (* ARRANGE *)
  let input : Ast.expr = Lam ("a", FunType ("x", Var "a", Var "x")) in
  let expected = Lam (FunType (Mono (Var 0), Var 0)) in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let nestedLambdaTypeAbstraction _ =
  (* ARRANGE *)
  let input : Ast.expr =
    Lam
      ( "a",
        Lam
          ( "b",
            Lam
              ( "c",
                FunType ("x", Var "b", Fun ("y", FunType ("z", Var "a", Int 1)))
              ) ) )
  in
  let expected =
    Lam
      (Lam (Lam (FunType (Mono (Var 1), Fun (FunType (Mono (Var 2), Int 1))))))
  in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let lambdaTypeWithForAll _ =
  (* ARRANGE *)
  let input : Ast.expr =
    Lam
      ( "a",
        FunType
          ( "x",
            ForAll ("a", ForAll ("b", ForAll ("c", Fun (Var "a", Var "b")))),
            Var "x" ) )
  in
  let expected =
    Lam (FunType (Poly (3, RhoMono (Fun (Var 2, Var 1))), Var 0))
  in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let nestedLambda _ =
  (* ARRANGE *)
  let input : Ast.expr =
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
                            Lam ("c", Lam ("d", FunType ("y", Var "a", Var "x")))
                          ) ) ) ) ) )
  in
  let expected =
    Lam
      (Lam
         (Lam
            (FunType
               ( Mono (Var 2),
                 Lam (Lam (Lam (Lam (FunType (Mono (Var 3), Var 1))))) ))))
  in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let toDeBruijnAnnot _ =
  (* ARRANGE *)
  let input : Ast.expr = Annot (Int 1, Int) in
  let expected = Annot (Int 1, Mono Int) in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let toDeBruijnList _ =
  (* ARRANGE *)
  let input : Ast.expr = List [ Int 1; Bool true ] in
  let expected = List [ Int 1; Bool true ] in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let toDeBruijnHead _ =
  (* ARRANGE *)
  let input : Ast.expr = Head (List [ Int 1; Bool true ]) in
  let expected = Head (List [ Int 1; Bool true ]) in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let toDeBruijnTail _ =
  (* ARRANGE *)
  let input : Ast.expr = Tail (List [ Int 1; Bool true ]) in
  let expected = Tail (List [ Int 1; Bool true ]) in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let toDeBruijnVarXExpectEmptyEnvError _ =
  (* ARRANGE *)
  let input : Ast.expr = Var "x" in
  let expected = DBAstException "Empty var env - unbound value x" in
  (* ACT *)
  let output _ = toDeBruijn input in
  (* ASSERT *)
  assert_raises expected output

let suite =
  "DeBruijnTransformTest"
  >::: [
         "toDeBruijnFunction" >:: toDeBruijnFunction;
         "toDeBruijnInt1" >:: toDeBruijnInt1;
         "toDeBruijnPairInt1Int2\n      " >:: toDeBruijnPairInt1Int2;
         "toDeBruijnFirst" >:: toDeBruijnFirst;
         "toDeBruijnNestedFunctions" >:: toDeBruijnNestedFunctions;
         "toDeBruijnNestedFunctionsWithApplication"
         >:: toDeBruijnNestedFunctionsWithApplication;
         "toDeBruijnNestedFunctionsWithApplication2"
         >:: toDeBruijnNestedFunctionsWithApplication2;
         "toDeBruijnApplication" >:: toDeBruijnApplication;
         "toDeBruijnUnit" >:: toDeBruijnUnit;
         "toDeBruijnBool" >:: toDeBruijnBool;
         "toDeBruijnPairUnitBoolFalse" >:: toDeBruijnPairUnitBoolFalse;
         "toDeBruijnSecond" >:: toDeBruijnSecond;
         "toDeBruijnExprInParenthesis" >:: toDeBruijnExprInParenthesis;
         "firstAndApplication" >:: firstAndApplication;
         "firstAndTypeApplication" >:: firstAndTypeApplication;
         "firstAndTypeApplication2" >:: firstAndTypeApplication2;
         "typedFunction" >:: typedFunction;
         "lambdaTypeAbstraction" >:: lambdaTypeAbstraction;
         "nestedLambdaTypeAbstraction" >:: nestedLambdaTypeAbstraction;
         "lambdaTypeWithForAll" >:: lambdaTypeWithForAll;
         "nestedLambda" >:: nestedLambda;
         "toDeBruijnAnnot" >:: toDeBruijnAnnot;
         "toDeBruijnList" >:: toDeBruijnList;
         "toDeBruijnHead" >:: toDeBruijnHead;
         "toDeBruijnTail" >:: toDeBruijnTail;
         "toDeBruijnVarXExpectEmptyEnvError"
         >:: toDeBruijnVarXExpectEmptyEnvError;
       ]

let intToString _ =
  (* ARRANGE *)
  let input = Int 1 in
  let expected = "1" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let varToString _ =
  (* ARRANGE *)
  let input = Var 0 in
  let expected = "p" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let boolToString _ =
  (* ARRANGE *)
  let input = Bool true in
  let expected = "true" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let unitToString _ =
  (* ARRANGE *)
  let input = Unit in
  let expected = "()" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let pairToString _ =
  (* ARRANGE *)
  let input = Pair (Int 2, Bool false) in
  let expected = "(2,false)" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let firstToString _ =
  (* ARRANGE *)
  let input = Fst (Int 1) in
  let expected = "fst 1" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let secondToString _ =
  (* ARRANGE *)
  let input = Snd Unit in
  let expected = "snd ()" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let funToString _ =
  (* ARRANGE *)
  let input = Fun (Var 0) in
  let expected = "fn q => q" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let funTypeToString _ =
  (* ARRANGE *)
  let input = FunType (Mono Int, Var 1) in
  let expected = "fn q : int => p" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let appToString _ =
  (* ARRANGE *)
  let input = App (Var 0, Var 1) in
  let expected = "(p) (o)" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let typeAppToString _ =
  (* ARRANGE *)
  let input = TypeApp (Var 0, Mono Bool) in
  let expected = "(p) {bool}" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let lamToString _ =
  (* ARRANGE *)
  let input = Lam (Pair (Unit, Unit)) in
  let expected = "lam a.((),())" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let annotToString _ =
  (* ARRANGE *)
  let input = Annot (Pair (Unit, Unit), Mono (Pair (Int, Int))) in
  let expected = "(((),())) : ((int, int))" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let listToString _ =
  (* ARRANGE *)
  let input = List [ Int 1; Unit; Var 0 ] in
  let expected = "[ 1 () p]" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let headToString _ =
  (* ARRANGE *)
  let input = Head (List [ Int 1; Unit ]) in
  let expected = "hd [ 1 ()]" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let tailToString _ =
  (* ARRANGE *)
  let input = Tail (List [ Int 1; Unit ]) in
  let expected = "tl [ 1 ()]" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let exprListToString _ =
  (* ARRANGE *)
  let input = [ Int 1; Unit; Var 0 ] in
  let expected = "1\n()\np\n" in
  (* ACT *)
  let output = exprListToString input in
  (* ASSERT *)
  assert_equal expected output

let suiteToString =
  "DBASTToStringTests"
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
         "headToString" >:: headToString;
         "tailToString" >:: tailToString;
         "exprListToString" >:: exprListToString;
       ]

let () = run_test_tt_main suite
let () = run_test_tt_main suiteToString
