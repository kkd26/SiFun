open Sifun
open DBType
open Subst
open Unify
open OUnit2

let basicTypes =
  [
    Int;
    Bool;
    Unit;
    FreshVar 0;
    FreshVar 1;
    FreshVar 2;
    FreshVar 3;
    FreshVar 4;
    FreshVar 5;
  ]

let constructors = [ (fun x y -> Fun (x, y)); (fun x y -> Pair (x, y)) ]

let combine l a =
  l
  @ List.flatten
      (List.map
         (fun x ->
           List.flatten (List.map (fun y -> List.map (fun z -> z x y) a) l))
         l)

let combinedTypes1 = combine basicTypes constructors

let combinedTypes2 =
  List.map (fun x -> Mono x) (combine combinedTypes1 constructors)

let compareSubstitutions s1 s2 =
  List.for_all
    (fun x -> applySubstToTypeGenre s1 x = applySubstToTypeGenre s2 x)
    combinedTypes2

let unifyHelper t1 t2 = snd (State.IntState.runState (unifyOne t1 t2) ~init:0)

let unifyIntWithInt _ =
  (* ARRANGE *)
  let expected = emptySubst in
  (* ACT *)
  let output = unifyHelper (Mono Int) (Mono Int) in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyIntWithBool _ =
  (* ARRANGE *)
  let expected = UnifyException "Cannot unify int and bool" in
  (* ACT *)
  let output _ = unifyHelper (Mono Int) (Mono Bool) in
  (* ASSERT *)
  assert_raises expected output

let unifyVar0WithVar1 _ =
  (* ARRANGE *)
  let expected = substFromList [ Mono (FreshVar 1) ] in
  (* ACT *)
  let output = unifyHelper (Mono (FreshVar 0)) (Mono (FreshVar 1)) in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyFunVar0BoolWithFunIntVar1 _ =
  (* ARRANGE *)
  let expected = substFromList [ Mono Int; Mono Bool ] in
  (* ACT *)
  let output =
    unifyHelper (Mono (Fun (FreshVar 0, Bool))) (Mono (Fun (Int, FreshVar 1)))
  in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyPairVar2BoolWithPairIntVar1 _ =
  (* ARRANGE *)
  let expected = substFromList [ Mono (FreshVar 0); Mono Bool; Mono Int ] in
  (* ACT *)
  let output =
    unifyHelper (Mono (Pair (FreshVar 2, Bool))) (Mono (Pair (Int, FreshVar 1)))
  in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyPairVar2BoolWithVar0 _ =
  (* ARRANGE *)
  let expected = substFromList [ Mono (Pair (FreshVar 2, Bool)) ] in
  (* ACT *)
  let output =
    unifyHelper (Mono (Pair (FreshVar 2, Bool))) (Mono (FreshVar 0))
  in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyFunFunVar0Var0Var0WithFunVar1Int _ =
  (* ARRANGE *)
  let expected = substFromList [ Mono Int; Mono (Fun (Int, Int)) ] in
  (* ACT *)
  let output =
    unifyHelper
      (Mono (Fun (Fun (FreshVar 0, FreshVar 0), FreshVar 0)))
      (Mono (Fun (FreshVar 1, Int)))
  in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyForAllInt _ =
  (* ARRANGE *)
  let expected = substFromList [ Mono Int ] in
  (* ACT *)
  let output =
    unifyHelper (Poly (1, RhoMono (Var 0))) (Poly (1, RhoMono Int))
  in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let inTypeMono _ =
  (* ARRANGE *)
  let input = List (FreshVar 0) in
  let expected = UnifyException "Circular dependencies in monoType " in
  (* ACT *)
  let output _ = inTypeMono 0 input in
  (* ASSERT *)
  assert_raises expected output

let inTypeRhoFun _ =
  (* ARRANGE *)
  let input = RhoFun ((0, RhoMono (FreshVar 1)), (2, RhoMono (FreshVar 4))) in
  let expected = () in
  (* ACT *)
  let output = inTypeRho 0 input in
  (* ASSERT *)
  assert_equal expected output

let inTypeRhoPair _ =
  (* ARRANGE *)
  let input = RhoPair ((0, RhoMono (FreshVar 1)), (2, RhoMono (FreshVar 4))) in
  let expected = () in
  (* ACT *)
  let output = inTypeRho 0 input in
  (* ASSERT *)
  assert_equal expected output

let inTypeRhoList _ =
  (* ARRANGE *)
  let input = RhoList (0, RhoMono (FreshVar 1)) in
  let expected = UnifyException "Circular dependencies in monoType " in
  (* ACT *)
  let output _ = inTypeRho 1 input in
  (* ASSERT *)
  assert_raises expected output

let unifyMonoTypeVar _ =
  (* ARRANGE *)
  let input1 = Var 0 in
  let input2 = Var 1 in
  let expected = UnifyException "Cannot unify different type vars" in
  (* ACT *)
  let output _ = unifyMono input1 input2 in
  (* ASSERT *)
  assert_raises expected output

let unifyMonoSameFreshVar _ =
  (* ARRANGE *)
  let input1 = Mono (FreshVar 1) in
  let input2 = Mono (FreshVar 1) in
  let expected = substFromList [] in
  (* ACT *)
  let output = unifyHelper input1 input2 in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyMonoFreshVarTypeVar _ =
  (* ARRANGE *)
  let input1 = FreshVar 1 in
  let input2 = Var 1 in
  let expected = UnifyException "Cannot unify freshvar with type var" in
  (* ACT *)
  let output _ = unifyMono input1 input2 in
  (* ASSERT *)
  assert_raises expected output

let unifyMonoTypeVarFreshVar _ =
  (* ARRANGE *)
  let input1 = Var 0 in
  let input2 = FreshVar 2 in
  let expected = UnifyException "Cannot unify freshvar with type var" in
  (* ACT *)
  let output _ = unifyMono input1 input2 in
  (* ASSERT *)
  assert_raises expected output

let unifyMonoFreshVarInt _ =
  (* ARRANGE *)
  let input1 = Mono (FreshVar 0) in
  let input2 = Mono Int in
  let expected = substFromList [ Mono Int ] in
  (* ACT *)
  let output = unifyHelper input1 input2 in
  (* ASSERT *)
  assert_bool "Incorrect" (compareSubstitutions expected output)

let unifyMonoListIntFreshVar _ =
  (* ARRANGE *)
  let input1 = FreshVar 0 in
  let input2 = Int in
  let expected = [ (0, Mono Int) ] in
  (* ACT *)
  let output =
    snd (State.IntState.runState (unifyMono input1 input2) ~init:0)
  in
  (* ASSERT *)
  assert_equal expected output

let unifyRhoFun _ =
  (* ARRANGE *)
  let input1 = RhoFun ((0, RhoMono (FreshVar 0)), (0, RhoMono (FreshVar 1))) in
  let input2 = RhoMono (Fun (Int, Bool)) in
  let expected = [ (0, Mono Int); (1, Mono Bool) ] in
  (* ACT *)
  let output = snd (State.IntState.runState (unifyRho input1 input2) ~init:0) in
  (* ASSERT *)
  assert_equal expected output

let unifyRhoPair1 _ =
  (* ARRANGE *)
  let input1 = RhoPair ((0, RhoMono (FreshVar 0)), (0, RhoMono (FreshVar 1))) in
  let input2 = RhoMono (Pair (Int, Bool)) in
  let expected = [ (0, Mono Int); (1, Mono Bool) ] in
  (* ACT *)
  let output = snd (State.IntState.runState (unifyRho input1 input2) ~init:0) in
  (* ASSERT *)
  assert_equal expected output

let unifyRhoPair2 _ =
  (* ARRANGE *)
  let input1 = RhoMono (Pair (Int, Bool)) in
  let input2 = RhoPair ((0, RhoMono (FreshVar 0)), (0, RhoMono (FreshVar 1))) in
  let expected = [ (0, Mono Int); (1, Mono Bool) ] in
  (* ACT *)
  let output = snd (State.IntState.runState (unifyRho input1 input2) ~init:0) in
  (* ASSERT *)
  assert_equal expected output

let unifyRhoPair3 _ =
  (* ARRANGE *)
  let input1 = RhoPair ((0, RhoMono (FreshVar 0)), (0, RhoMono (FreshVar 1))) in
  let input2 = RhoPair ((0, RhoMono (FreshVar 0)), (0, RhoMono (FreshVar 1))) in
  let expected = [] in
  (* ACT *)
  let output = snd (State.IntState.runState (unifyRho input1 input2) ~init:0) in
  (* ASSERT *)
  assert_equal expected output

let unifyRhoList1 _ =
  (* ARRANGE *)
  let input1 = RhoList (0, RhoMono (FreshVar 0)) in
  let input2 = RhoMono (List Int) in
  let expected = [ (0, Mono Int) ] in
  (* ACT *)
  let output = snd (State.IntState.runState (unifyRho input1 input2) ~init:0) in
  (* ASSERT *)
  assert_equal expected output

let unifyRhoList2 _ =
  (* ARRANGE *)
  let input1 = RhoMono (List Int) in
  let input2 = RhoList (0, RhoMono (FreshVar 0)) in
  let expected = [ (0, Mono Int) ] in
  (* ACT *)
  let output = snd (State.IntState.runState (unifyRho input1 input2) ~init:0) in
  (* ASSERT *)
  assert_equal expected output

let unifyRhoList3 _ =
  (* ARRANGE *)
  let input1 = RhoList (0, RhoMono (FreshVar 0)) in
  let input2 = RhoList (0, RhoMono Int) in
  let expected = [ (0, Mono Int) ] in
  (* ACT *)
  let output = snd (State.IntState.runState (unifyRho input1 input2) ~init:0) in
  (* ASSERT *)
  assert_equal expected output

let unifyRhoMono _ =
  (* ARRANGE *)
  let input1 = RhoMono (FreshVar 0) in
  let input2 = RhoMono Int in
  let expected = [ (0, Mono Int) ] in
  (* ACT *)
  let output = snd (State.IntState.runState (unifyRho input1 input2) ~init:0) in
  (* ASSERT *)
  assert_equal expected output

let unifyRhoException _ =
  (* ARRANGE *)
  let input1 = RhoMono (FreshVar 0) in
  let input2 = RhoList (0, RhoMono Int) in
  let expected = UnifyException "Cannot unify f0 and int list" in
  (* ACT *)
  let output _ =
    snd (State.IntState.runState (unifyRho input1 input2) ~init:0)
  in
  (* ASSERT *)
  assert_raises expected output

let unifyPolyException _ =
  (* ARRANGE *)
  let input1 = (0, RhoMono (FreshVar 0)) in
  let input2 = (1, RhoList (0, RhoMono Int)) in
  let expected = UnifyException "Different size f0 and forall a1. (int list)" in
  (* ACT *)
  let output _ =
    snd (State.IntState.runState (unifyPoly input1 input2) ~init:0)
  in
  (* ASSERT *)
  assert_raises expected output

let unifyIntWithPolyInt _ =
  (* ARRANGE *)
  let expected =
    UnifyException
      "Different size forall a1. (int) list and forall a1. (int list)"
  in
  (* ACT *)
  let output _ =
    unifyHelper (Rho (RhoList (1, RhoMono Int))) (Poly (1, RhoMono (List Int)))
  in
  (* ASSERT *)
  assert_raises expected output

let suite =
  "UnifyTest"
  >::: [
         "unifyIntWithInt" >:: unifyIntWithInt;
         "unifyIntWithBool" >:: unifyIntWithBool;
         "unifyVar0WithVar1" >:: unifyVar0WithVar1;
         "unifyFunVar0BoolWithFunIntVar1" >:: unifyFunVar0BoolWithFunIntVar1;
         "unifyPairVar2BoolWithPairIntVar1" >:: unifyPairVar2BoolWithPairIntVar1;
         "unifyPairVar2BoolWithVar0" >:: unifyPairVar2BoolWithVar0;
         "unifyFunFunVar0Var0Var0WithFunVar1Int"
         >:: unifyFunFunVar0Var0Var0WithFunVar1Int;
         "unifyForAllInt" >:: unifyForAllInt;
         "inTypeMono" >:: inTypeMono;
         "inTypeRhoFun" >:: inTypeRhoFun;
         "inTypeRhoPair" >:: inTypeRhoPair;
         "inTypeRhoList" >:: inTypeRhoList;
         "unifyMonoTypeVar" >:: unifyMonoTypeVar;
         "unifyMonoSameFreshVar" >:: unifyMonoSameFreshVar;
         "unifyMonoFreshVarTypeVar" >:: unifyMonoFreshVarTypeVar;
         "unifyMonoTypeVarFreshVar" >:: unifyMonoTypeVarFreshVar;
         "unifyMonoFreshVarInt" >:: unifyMonoFreshVarInt;
         "unifyMonoListIntFreshVar" >:: unifyMonoListIntFreshVar;
         "unifyRhoFun" >:: unifyRhoFun;
         "unifyRhoPair1" >:: unifyRhoPair1;
         "unifyRhoPair2" >:: unifyRhoPair2;
         "unifyRhoPair3" >:: unifyRhoPair3;
         "unifyRhoList1" >:: unifyRhoList1;
         "unifyRhoList2" >:: unifyRhoList2;
         "unifyRhoList3" >:: unifyRhoList3;
         "unifyRhoMono" >:: unifyRhoMono;
         "unifyRhoException" >:: unifyRhoException;
         "unifyPolyException" >:: unifyPolyException;
         "unifyIntWithPolyInt" >:: unifyIntWithPolyInt;
       ]

let () = run_test_tt_main suite
