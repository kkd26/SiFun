open Sifun
open Debruijn
open DBType
open Infer
open OUnit2

let inferUnit _ =
  (* ARRANGE *)
  let input : Debruijn.expr = Unit in
  let expected = Unit in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let inferBool _ =
  (* ARRANGE *)
  let input : Debruijn.expr = Bool true in
  let expected = Bool in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let inferInt1 _ =
  (* ARRANGE *)
  let input : Debruijn.expr = Int 1 in
  let expected = Int in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let inferPairInt1Int2 _ =
  (* ARRANGE *)
  let input : Debruijn.expr = Pair (Int 1, Int 2) in
  let expected = Pair (Int, Int) in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let inferPairUnitBoolFalse _ =
  (* ARRANGE *)
  let input : Debruijn.expr = Pair (Unit, Bool false) in
  let expected = Pair (Unit, Bool) in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let inferFunction _ =
  (* ARRANGE *)
  let input : Debruijn.expr = Fun (Var 0) in
  let expected = Fun (FreshVar 0, FreshVar 0) in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let inferFirst _ =
  (* ARRANGE *)
  let input : Debruijn.expr = Fst (Pair (Int 1, Int 2)) in
  let expected = Int in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let inferSecond _ =
  (* ARRANGE *)
  let input : Debruijn.expr = Snd (Pair (Bool true, Bool false)) in
  let expected = Bool in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let inferNestedFunctions _ =
  (* ARRANGE *)
  let input : Debruijn.expr = Fun (Fun (Fun (Var 0))) in
  let expected =
    Fun (FreshVar 0, Fun (FreshVar 1, Fun (FreshVar 2, FreshVar 2)))
  in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let inferNestedFunctionsWithApplication _ =
  (* ARRANGE *)
  let input : Debruijn.expr = Fun (Fun (Fun (Fun (App (Var 0, Var 3))))) in
  let expected =
    Fun
      ( FreshVar 0,
        Fun
          ( FreshVar 1,
            Fun (FreshVar 2, Fun (Fun (FreshVar 0, FreshVar 4), FreshVar 4)) )
      )
  in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let inferNestedFunctionsWithApplication2 _ =
  (* ARRANGE *)
  let input : Debruijn.expr = Fun (Fun (Fun (App (Fun (Var 0), Var 2)))) in
  let expected =
    Fun (FreshVar 3, Fun (FreshVar 1, Fun (FreshVar 2, FreshVar 3)))
  in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let inferApplication _ =
  (* ARRANGE *)
  let input : Debruijn.expr =
    Fun (Fun (Fun (App (App (Var 2, Var 1), Var 0))))
  in
  let expected =
    Fun
      ( Fun (FreshVar 1, Fun (FreshVar 2, FreshVar 4)),
        Fun (FreshVar 1, Fun (FreshVar 2, FreshVar 4)) )
  in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let inferExprInParenthesis _ =
  (* ARRANGE *)
  let input : Debruijn.expr = App (Int 1, Int 2) in
  let expected = Failure "Cannot unify int -> f0 and int" in
  (* ACT *)
  let output _ = snd (inferType input) in
  (* ASSERT *)
  assert_raises expected output

let firstAndApplication _ =
  (* ARRANGE *)
  let input : Debruijn.expr = Fun (Fun (App (Fst (Var 1), Var 0))) in
  let expected =
    Fun
      ( Pair (Fun (FreshVar 1, FreshVar 4), FreshVar 3),
        Fun (FreshVar 1, FreshVar 4) )
  in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let firstAndTypeApplication _ =
  (* ARRANGE *)
  let input : Debruijn.expr = Fun (TypeApp (Fst (Var 0), Int)) in
  let expected = Int in
  (* ACT *)
  let _ = snd (inferTypeHMV input) in
  (* ASSERT *)
  assert_equal expected expected

let firstAndTypeApplication2 _ =
  (* ARRANGE *)
  let input : Debruijn.expr =
    Fun (TypeApp (TypeApp (Fst (Var 0), Int), Bool))
  in
  let expected = Int in
  (* ACT *)
  let _ = snd (inferTypeHMV input) in
  (* ASSERT *)
  assert_equal expected expected

let typedFunction _ =
  (* ARRANGE *)
  let input : Debruijn.expr = FunType (Int, Var 0) in
  let expected = Fun (Int, Int) in
  (* ACT *)
  let output = snd (inferTypeHMV input) in
  (* ASSERT *)
  assert_equal expected output

let lambdaTypeAbstraction _ =
  (* ARRANGE *)
  let input : Debruijn.expr = Lam (FunType (Var 0, Var 0)) in
  let expected = ForAll (Fun (Var 0, Var 0)) in
  (* ACT *)
  let output = snd (inferTypeHMV input) in
  (* ASSERT *)
  assert_equal expected output

let nestedLambdaTypeAbstraction _ =
  (* ARRANGE *)
  let input : Debruijn.expr =
    Lam (Lam (Lam (FunType (Var 1, Fun (FunType (Var 2, Int 1))))))
  in
  let expected =
    ForAll (ForAll (ForAll (Fun (Var 1, Fun (FreshVar 0, Fun (Var 2, Int))))))
  in
  (* ACT *)
  let output = snd (inferTypeHMV input) in
  (* ASSERT *)
  assert_equal expected output

let lambdaTypeWithForAll _ =
  (* ARRANGE *)
  let input : Debruijn.expr =
    Lam (FunType (ForAll (ForAll (ForAll (Fun (Var 2, Var 1)))), Var 0))
  in
  let expected =
    ForAll
      (Fun
         ( ForAll (ForAll (ForAll (Fun (Var 2, Var 1)))),
           ForAll (ForAll (ForAll (Fun (Var 2, Var 1)))) ))
  in
  (* ACT *)
  let output = snd (inferTypeHMV input) in
  (* ASSERT *)
  assert_equal expected output

let nestedLambda _ =
  (* ARRANGE *)
  let input : Debruijn.expr =
    Lam
      (Lam
         (Lam (FunType (Var 2, Lam (Lam (Lam (Lam (FunType (Var 3, Var 1)))))))))
  in
  let expected =
    ForAll
      (ForAll
         (ForAll
            (Fun (Var 2, ForAll (ForAll (ForAll (ForAll (Fun (Var 3, Var 6)))))))))
  in
  (* ACT *)
  let output = snd (inferTypeHMV input) in
  (* ASSERT *)
  assert_equal expected output

let suite =
  "DeBruijnTransformTest"
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
       ]

let () = run_test_tt_main suite
