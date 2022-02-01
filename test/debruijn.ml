open Sifun
open Ast
open Debruijn
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
  let expected = Fun (TypeApp (Fst (Var 0), Int)) in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let firstAndTypeApplication2 _ =
  (* ARRANGE *)
  let input : Ast.expr =
    Fun ("x", TypeApp (TypeApp (Fst (Var "x"), Int), Bool))
  in
  let expected = Fun (TypeApp (TypeApp (Fst (Var 0), Int), Bool)) in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let typedFunction _ =
  (* ARRANGE *)
  let input : Ast.expr = FunType ("x", Int, Var "x") in
  let expected = FunType (Int, Var 0) in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

let lambdaTypeAbstraction _ =
  (* ARRANGE *)
  let input : Ast.expr = Lam ("a", FunType ("x", Var "a", Var "x")) in
  let expected = Lam (FunType (Var 0, Var 0)) in
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
    Lam (Lam (Lam (FunType (Var 1, Fun (FunType (Var 2, Int 1))))))
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
    Lam (FunType (ForAll (ForAll (ForAll (Fun (Var 2, Var 1)))), Var 0))
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
         (Lam (FunType (Var 2, Lam (Lam (Lam (Lam (FunType (Var 3, Var 1)))))))))
  in
  (* ACT *)
  let output = toDeBruijn input in
  (* ASSERT *)
  assert_equal expected output

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
       ]

let () = run_test_tt_main suite
