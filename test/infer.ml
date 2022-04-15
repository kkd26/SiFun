open Sifun
open DBAst
open DBType
open Infer
open OUnit2

let inferUnit _ =
  (* ARRANGE *)
  let input : DBAst.expr = Unit in
  let expected = Mono Unit in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let inferBool _ =
  (* ARRANGE *)
  let input : DBAst.expr = Bool true in
  let expected = Mono Bool in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let inferInt1 _ =
  (* ARRANGE *)
  let input : DBAst.expr = Int 1 in
  let expected = Mono Int in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let inferPairInt1Int2 _ =
  (* ARRANGE *)
  let input : DBAst.expr = Pair (Int 1, Int 2) in
  let expected = Mono (Pair (Int, Int)) in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let inferPairUnitBoolFalse _ =
  (* ARRANGE *)
  let input : DBAst.expr = Pair (Unit, Bool false) in
  let expected = Mono (Pair (Unit, Bool)) in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let inferFunction _ =
  (* ARRANGE *)
  let input : DBAst.expr = Fun (Var 0) in
  let expected = Mono (Fun (FreshVar 0, FreshVar 0)) in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let inferFirst _ =
  (* ARRANGE *)
  let input : DBAst.expr = Fst (Pair (Int 1, Int 2)) in
  let expected = Mono Int in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let inferSecond _ =
  (* ARRANGE *)
  let input : DBAst.expr = Snd (Pair (Bool true, Bool false)) in
  let expected = Mono Bool in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let inferNestedFunctions _ =
  (* ARRANGE *)
  let input : DBAst.expr = Fun (Fun (Fun (Var 0))) in
  let expected =
    Mono (Fun (FreshVar 0, Fun (FreshVar 1, Fun (FreshVar 2, FreshVar 2))))
  in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let inferNestedFunctionsWithApplication _ =
  (* ARRANGE *)
  let input : DBAst.expr = Fun (Fun (Fun (Fun (App (Var 0, Var 3))))) in
  let expected =
    Mono
      (Fun
         ( FreshVar 0,
           Fun
             ( FreshVar 1,
               Fun (FreshVar 2, Fun (Fun (FreshVar 0, FreshVar 4), FreshVar 4))
             ) ))
  in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let inferNestedFunctionsWithApplication2 _ =
  (* ARRANGE *)
  let input : DBAst.expr = Fun (Fun (Fun (App (Fun (Var 0), Var 2)))) in
  let expected =
    Mono (Fun (FreshVar 3, Fun (FreshVar 1, Fun (FreshVar 2, FreshVar 3))))
  in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let inferApplication _ =
  (* ARRANGE *)
  let input : DBAst.expr =
    Fun (Fun (Fun (App (App (Var 2, Var 1), Var 0))))
  in
  let expected =
    Mono
      (Fun
         ( Fun (FreshVar 1, Fun (FreshVar 2, FreshVar 4)),
           Fun (FreshVar 1, Fun (FreshVar 2, FreshVar 4)) ))
  in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let inferExprInParenthesis _ =
  (* ARRANGE *)
  let input : DBAst.expr = App (Int 1, Int 2) in
  let expected = Unify.UnifyException "Cannot unify int -> f0 and int" in
  (* ACT *)
  let output _ = snd (inferType input) in
  (* ASSERT *)
  assert_raises expected output

let firstAndApplication _ =
  (* ARRANGE *)
  let input : DBAst.expr = Fun (Fun (App (Fst (Var 1), Var 0))) in
  let expected =
    Mono
      (Fun
         ( Pair (Fun (FreshVar 1, FreshVar 4), FreshVar 3),
           Fun (FreshVar 1, FreshVar 4) ))
  in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let firstAndTypeApplication _ =
  (* ARRANGE *)
  let input : DBAst.expr = Fun (TypeApp (Fst (Var 0), Mono Int)) in
  let expected =
    Rho
      (F ((0, P ((1, T (FreshVar 3)), (0, T (FreshVar 2)))), (0, T (FreshVar 3))))
  in
  (* ACT *)
  let output = snd (inferTypeHMV input) in
  (* ASSERT *)
  assert_equal expected output

let firstAndTypeApplication2 _ =
  (* ARRANGE *)
  let input : DBAst.expr =
    Fun (TypeApp (TypeApp (Fst (Var 0), Mono Int), Mono Bool))
  in
  let expected =
    Rho
      (F ((0, P ((2, T (FreshVar 4)), (0, T (FreshVar 2)))), (0, T (FreshVar 4))))
  in
  (* ACT *)
  let output = snd (inferTypeHMV input) in
  (* ASSERT *)
  assert_equal expected output

let typedFunction _ =
  (* ARRANGE *)
  let input : DBAst.expr = FunType (Mono Int, Var 0) in
  let expected = Mono (Fun (Int, Int)) in
  (* ACT *)
  let output = snd (inferTypeHMV input) in
  (* ASSERT *)
  assert_equal expected output

let lambdaTypeAbstraction _ =
  (* ARRANGE *)
  let input : DBAst.expr = Lam (FunType (Mono (Var 0), Var 0)) in
  let expected = Poly (1, T (Fun (Var 0, Var 0))) in
  (* ACT *)
  let output = snd (inferTypeHMV input) in
  (* ASSERT *)
  assert_equal expected output

let nestedLambdaTypeAbstraction _ =
  (* ARRANGE *)
  let input : DBAst.expr =
    Lam
      (Lam (Lam (FunType (Mono (Var 1), Fun (FunType (Mono (Var 2), Int 1))))))
  in
  let expected =
    Poly (3, T (Fun (Var 1, Fun (FreshVar 0, Fun (Var 2, Int)))))
  in
  (* ACT *)
  let output = snd (inferTypeHMV input) in
  (* ASSERT *)
  assert_equal expected output

let lambdaTypeWithForAll _ =
  (* ARRANGE *)
  let input : DBAst.expr =
    Lam (FunType (Poly (3, T (Fun (Var 2, Var 1))), Var 0))
  in
  let expected =
    Poly (1, F ((3, T (Fun (Var 2, Var 1))), (3, T (Fun (Var 2, Var 1)))))
  in
  (* ACT *)
  let output = snd (inferTypeHMV input) in
  (* ASSERT *)
  assert_equal expected output

let nestedLambda _ =
  (* ARRANGE *)
  let input : DBAst.expr =
    Lam
      (Lam
         (Lam
            (FunType
               ( Mono (Var 2),
                 Lam (Lam (Lam (Lam (FunType (Mono (Var 3), Var 1))))) ))))
  in
  let expected = Poly (3, F ((0, T (Var 2)), (4, T (Fun (Var 3, Var 6))))) in
  (* ACT *)
  let output = snd (inferTypeHMV input) in
  (* ASSERT *)
  assert_equal expected output

let funPairTypedApp1True _ =
  (* ARRANGE *)
  let input : DBAst.expr =
    FunType
      ( Poly (1, T (Fun (Var 0, Var 0))),
        Pair
          ( App (TypeApp (Var 0, Mono Int), Int 1),
            App (TypeApp (Var 0, Mono Bool), Bool true) ) )
  in
  let expected =
    Rho (F ((1, T (Fun (Var 0, Var 0))), (0, T (Pair (Int, Bool)))))
  in
  (* ACT *)
  let output = snd (inferTypeHMV input) in
  (* ASSERT *)
  assert_equal expected output

let appPairApp1TrueIdentity _ =
  (* ARRANGE *)
  let input : DBAst.expr =
    App
      ( FunType
          ( Poly (1, T (Fun (Var 0, Var 0))),
            Pair
              ( App (TypeApp (Var 0, Mono Int), Int 1),
                App (TypeApp (Var 0, Mono Bool), Bool true) ) ),
        Lam (FunType (Mono (Var 0), Var 0)) )
  in
  let expected = Mono (Pair (Int, Bool)) in
  (* ACT *)
  let output = snd (inferTypeHMV input) in
  (* ASSERT *)
  assert_equal expected output

let suite =
  "TypeInferenceTest"
  >::: [
         "inferFunction" >:: inferFunction;
         "inferInt1" >:: inferInt1;
         "inferPairInt1Int2\n      " >:: inferPairInt1Int2;
         "inferFirst" >:: inferFirst;
         "inferNestedFunctions" >:: inferNestedFunctions;
         "inferNestedFunctionsWithApplication"
         >:: inferNestedFunctionsWithApplication;
         "inferNestedFunctionsWithApplication2"
         >:: inferNestedFunctionsWithApplication2;
         "inferApplication" >:: inferApplication;
         "inferUnit" >:: inferUnit;
         "inferBool" >:: inferBool;
         "inferPairUnitBoolFalse" >:: inferPairUnitBoolFalse;
         "inferSecond" >:: inferSecond;
         "inferExprInParenthesis" >:: inferExprInParenthesis;
         "firstAndApplication" >:: firstAndApplication;
         "firstAndTypeApplication" >:: firstAndTypeApplication;
         "firstAndTypeApplication2" >:: firstAndTypeApplication2;
         "typedFunction" >:: typedFunction;
         "lambdaTypeAbstraction" >:: lambdaTypeAbstraction;
         "nestedLambdaTypeAbstraction" >:: nestedLambdaTypeAbstraction;
         "lambdaTypeWithForAll" >:: lambdaTypeWithForAll;
         "nestedLambda" >:: nestedLambda;
         "funPairTypedApp1True" >:: funPairTypedApp1True;
         "appPairApp1TrueIdentity" >:: appPairApp1TrueIdentity;
       ]

let () = run_test_tt_main suite
