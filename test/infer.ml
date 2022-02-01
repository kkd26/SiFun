open Sifun
open Debruijn
open DBType
open Infer
open OUnit2

let parseUnit _ =
  (* ARRANGE *)
  let input : Debruijn.expr = Unit in
  let expected = Unit in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let parseBool _ =
  (* ARRANGE *)
  let input : Debruijn.expr = Bool true in
  let expected = Bool in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let parseInt1 _ =
  (* ARRANGE *)
  let input : Debruijn.expr = Int 1 in
  let expected = Int in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let parsePairInt1Int2 _ =
  (* ARRANGE *)
  let input : Debruijn.expr = Pair (Int 1, Int 2) in
  let expected = Pair (Int, Int) in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let parsePairUnitBoolFalse _ =
  (* ARRANGE *)
  let input : Debruijn.expr = Pair (Unit, Bool false) in
  let expected = Pair (Unit, Bool) in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let parseFunction _ =
  (* ARRANGE *)
  let input : Debruijn.expr = Fun (Var 0) in
  let expected = Fun (FreshVar 0, FreshVar 0) in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let parseFirst _ =
  (* ARRANGE *)
  let input : Debruijn.expr = Fst (Pair (Int 1, Int 2)) in
  let expected = Int in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let parseSecond _ =
  (* ARRANGE *)
  let input : Debruijn.expr = Snd (Pair (Bool true, Bool false)) in
  let expected = Bool in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let parseNestedFunctions _ =
  (* ARRANGE *)
  let input : Debruijn.expr = Fun (Fun (Fun (Var 0))) in
  let expected =
    Fun (FreshVar 0, Fun (FreshVar 1, Fun (FreshVar 2, FreshVar 2)))
  in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let parseNestedFunctionsWithApplication _ =
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

let parseNestedFunctionsWithApplication2 _ =
  (* ARRANGE *)
  let input : Debruijn.expr = Fun (Fun (Fun (App (Fun (Var 0), Var 2)))) in
  let expected =
    Fun (FreshVar 0, Fun (FreshVar 1, Fun (FreshVar 2, FreshVar 0)))
  in
  (* ACT *)
  let output = snd (inferType input) in
  (* ASSERT *)
  assert_equal expected output

let parseApplication _ =
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

let parseExprInParenthesis _ =
  (* ARRANGE *)
  let input : Debruijn.expr = App (Int 1, Int 2) in
  let expected = Failure "Cannot unify int and int -> f0" in
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
