open Sifun
open DBType
open OUnit2

let typeGenreToMonoBool _ =
  (* ARRANGE *)
  let input = Mono Bool in
  let expected = Bool in
  (* ACT *)
  let output = typeGenreToMono input in
  (* ASSERT *)
  assert_equal expected output

let typeGenreToMonoRhoMonoInt _ =
  (* ARRANGE *)
  let input = Rho (RhoMono Int) in
  let expected = Int in
  (* ACT *)
  let output = typeGenreToMono input in
  (* ASSERT *)
  assert_equal expected output

let typeGenreToMonoPolyInt _ =
  (* ARRANGE *)
  let input = Poly (0, RhoMono Int) in
  let expected = Int in
  (* ACT *)
  let output = typeGenreToMono input in
  (* ASSERT *)
  assert_equal expected output

let typeGenreToMonoPolyException _ =
  (* ARRANGE *)
  let input = Poly (1, RhoMono Int) in
  let expected =
    DBTypeException "Conversion tk to mono not possible forall a. (int)"
  in
  (* ACT *)
  let output _ = typeGenreToMono input in
  (* ASSERT *)
  assert_raises expected output

let typeGenreToRhoPoly0 _ =
  (* ARRANGE *)
  let input = Poly (0, RhoMono (Pair (Int, Bool))) in
  let expected = RhoMono (Pair (Int, Bool)) in
  (* ACT *)
  let output = typeGenreToRho input in
  (* ASSERT *)
  assert_equal expected output

let typeGenreToRhoException _ =
  (* ARRANGE *)
  let input = Poly (1, RhoMono (Pair (Int, Bool))) in
  let expected = DBTypeException "Conversion tk to rho not possible" in
  (* ACT *)
  let output _ = typeGenreToRho input in
  (* ASSERT *)
  assert_raises expected output

let typeGenreToPoly _ =
  (* ARRANGE *)
  let input = Mono Int in
  let expected = (0, RhoMono Int) in
  (* ACT *)
  let output = typeGenreToPoly input in
  (* ASSERT *)
  assert_equal expected output

let normalizeRhoListMonoInt _ =
  (* ARRANGE *)
  let input = Rho (RhoList (0, RhoMono Int)) in
  let expected = Mono (List Int) in
  (* ACT *)
  let output = normalize input in
  (* ASSERT *)
  assert_equal expected output

let isMonoTypePairForAllIntIntExpectFalse _ =
  (* ARRANGE *)
  let input = Type.Pair (Type.ForAll ("a", Int), Type.Int) in
  let expected = false in
  (* ACT *)
  let output = isMonoType input in
  (* ASSERT *)
  assert_equal expected output

let isMonoTypeList _ =
  (* ARRANGE *)
  let input = Type.Pair (Type.List Type.Bool, Type.Int) in
  let expected = true in
  (* ACT *)
  let output = isMonoType input in
  (* ASSERT *)
  assert_equal expected output

let typeToDeBruijnRho _ =
  (* ARRANGE *)
  let input = Type.Pair (Type.ForAll ("a", Var "a"), Type.Int) in
  let expected = Rho (RhoPair ((1, RhoMono (Var 0)), (0, RhoMono Int))) in
  (* ACT *)
  let output = typeToDeBruijn (DBAst.emptyEnv "Empty var env") input in
  (* ASSERT *)
  assert_equal expected output

let typeToDeBruijnRho2 _ =
  (* ARRANGE *)
  let input =
    Type.Pair
      (Type.ForAll ("a", Type.Fun (Var "a", Type.List Type.Unit)), Type.Bool)
  in
  let expected =
    Rho (RhoPair ((1, RhoMono (Fun (Var 0, List Unit))), (0, RhoMono Bool)))
  in
  (* ACT *)
  let output = typeToDeBruijn (DBAst.emptyEnv "Empty var env") input in
  (* ASSERT *)
  assert_equal expected output

let monoTypeToDeBruijnPairIntBool _ =
  (* ARRANGE *)
  let input = Type.Pair (Int, Bool) in
  let expected = Mono (Pair (Int, Bool)) in
  (* ACT *)
  let output = monoTypeToDeBruijn (DBAst.emptyEnv "Empty var env") input in
  (* ASSERT *)
  assert_equal expected output

let monoTypeToDeBruijnException _ =
  (* ARRANGE *)
  let input = Type.ForAll ("a", Int) in
  let expected = DBTypeException "Not a monoType" in
  (* ACT *)
  let output _ = monoTypeToDeBruijn (DBAst.emptyEnv "Empty var env") input in
  (* ASSERT *)
  assert_raises expected output

let rhoTypeToDeBruijnPairIntForAll _ =
  (* ARRANGE *)
  let input = Type.Fun (Int, ForAll ("a", Int)) in
  let expected = Rho (RhoFun ((0, RhoMono Int), (1, RhoMono Int))) in
  (* ACT *)
  let output = rhoTypeToDeBruijn (DBAst.emptyEnv "Empty var env") input in
  (* ASSERT *)
  assert_equal expected output

let rhoTypeToDeBruijnList _ =
  (* ARRANGE *)
  let input = Type.List (ForAll ("a", Int)) in
  let expected = Rho (RhoList (1, RhoMono Int)) in
  (* ACT *)
  let output = rhoTypeToDeBruijn (DBAst.emptyEnv "Empty var env") input in
  (* ASSERT *)
  assert_equal expected output

let rhoTypeToDeBruijnException _ =
  (* ARRANGE *)
  let input = Type.ForAll ("a", Int) in
  let expected = DBTypeException "Not a rhoType" in
  (* ACT *)
  let output _ = rhoTypeToDeBruijn (DBAst.emptyEnv "Empty var env") input in
  (* ASSERT *)
  assert_raises expected output

let shiftMonoInt _ =
  (* ARRANGE *)
  let input = Int in
  let expected = Int in
  (* ACT *)
  let output = shiftMono 1 0 input in
  (* ASSERT *)
  assert_equal expected output

let shiftMonoBool _ =
  (* ARRANGE *)
  let input = Bool in
  let expected = Bool in
  (* ACT *)
  let output = shiftMono 1 0 input in
  (* ASSERT *)
  assert_equal expected output

let shiftMonoUnit _ =
  (* ARRANGE *)
  let input = Unit in
  let expected = Unit in
  (* ACT *)
  let output = shiftMono 1 0 input in
  (* ASSERT *)
  assert_equal expected output

let shiftMonoPair _ =
  (* ARRANGE *)
  let input = Pair (Int, Bool) in
  let expected = Pair (Int, Bool) in
  (* ACT *)
  let output = shiftMono 1 0 input in
  (* ASSERT *)
  assert_equal expected output

let shiftMonoList _ =
  (* ARRANGE *)
  let input = List Int in
  let expected = List Int in
  (* ACT *)
  let output = shiftMono 1 0 input in
  (* ASSERT *)
  assert_equal expected output

let shiftRhoPair _ =
  (* ARRANGE *)
  let input = RhoPair ((0, RhoMono Int), (0, RhoMono Bool)) in
  let expected = RhoPair ((0, RhoMono Int), (0, RhoMono Bool)) in
  (* ACT *)
  let output = shiftRho 1 0 input in
  (* ASSERT *)
  assert_equal expected output

let shiftRhoList _ =
  (* ARRANGE *)
  let input = RhoList (0, RhoMono Int) in
  let expected = RhoList (0, RhoMono Int) in
  (* ACT *)
  let output = shiftRho 1 0 input in
  (* ASSERT *)
  assert_equal expected output

let shiftTypePoly4Var2 _ =
  (* ARRANGE *)
  let input = Poly (4, RhoMono (Var 2)) in
  let expected = Poly (4, RhoMono (Var 2)) in
  (* ACT *)
  let output = shiftType 1 0 input in
  (* ASSERT *)
  assert_equal expected output

let substMonoInt _ =
  (* ARRANGE *)
  let input = Int in
  let e = Mono Int in
  let expected = Mono Int in
  (* ACT *)
  let output = substMono e 1 input in
  (* ASSERT *)
  assert_equal expected output

let substMonoBool _ =
  (* ARRANGE *)
  let input = Bool in
  let e = Mono Bool in
  let expected = Mono Bool in
  (* ACT *)
  let output = substMono e 0 input in
  (* ASSERT *)
  assert_equal expected output

let substMonoUnit _ =
  (* ARRANGE *)
  let input = Unit in
  let e = Mono Unit in
  let expected = Mono Unit in
  (* ACT *)
  let output = substMono e 0 input in
  (* ASSERT *)
  assert_equal expected output

let substMonoPair _ =
  (* ARRANGE *)
  let input = Pair (Int, Bool) in
  let e = Mono Unit in
  let expected = Rho (RhoPair ((0, RhoMono Int), (0, RhoMono Bool))) in
  (* ACT *)
  let output = substMono e 0 input in
  (* ASSERT *)
  assert_equal expected output

let substMonoList _ =
  (* ARRANGE *)
  let input = List Int in
  let e = Mono Unit in
  let expected = Rho (RhoList (0, RhoMono Int)) in
  (* ACT *)
  let output = substMono e 0 input in
  (* ASSERT *)
  assert_equal expected output

let substRhoFun _ =
  (* ARRANGE *)
  let input = RhoFun ((0, RhoMono Int), (1, RhoMono (Var 1))) in
  let e = Mono Unit in
  let expected = Rho (RhoFun ((0, RhoMono Int), (1, RhoMono Unit))) in
  (* ACT *)
  let output = substRho e 0 input in
  (* ASSERT *)
  assert_equal expected output

let substRhoPair _ =
  (* ARRANGE *)
  let input = RhoPair ((0, RhoMono Int), (1, RhoMono (Var 1))) in
  let e = Mono Unit in
  let expected = Rho (RhoPair ((0, RhoMono Int), (1, RhoMono Unit))) in
  (* ACT *)
  let output = substRho e 0 input in
  (* ASSERT *)
  assert_equal expected output

let substRhoList _ =
  (* ARRANGE *)
  let input = RhoList (1, RhoMono (Var 1)) in
  let e = Mono Unit in
  let expected = Rho (RhoList (1, RhoMono Unit)) in
  (* ACT *)
  let output = substRho e 0 input in
  (* ASSERT *)
  assert_equal expected output

let substPolyPoly _ =
  (* ARRANGE *)
  let input = (2, RhoMono (Var 2)) in
  let e = Poly (2, RhoMono Int) in
  let expected = Poly (4, RhoMono Int) in
  (* ACT *)
  let output = substPoly e 0 input in
  (* ASSERT *)
  assert_equal expected output

let substPolyRho _ =
  (* ARRANGE *)
  let input = (2, RhoMono (Var 2)) in
  let e = Rho (RhoMono Int) in
  let expected = Poly (2, RhoMono Int) in
  (* ACT *)
  let output = substPoly e 0 input in
  (* ASSERT *)
  assert_equal expected output

let substTypePoly _ =
  (* ARRANGE *)
  let input = Poly (2, RhoMono (Var 2)) in
  let e = Rho (RhoMono Int) in
  let expected = Poly (2, RhoMono Int) in
  (* ACT *)
  let output = substType e 0 input in
  (* ASSERT *)
  assert_equal expected output

let substTypeRho _ =
  (* ARRANGE *)
  let input = Rho (RhoMono (Var 0)) in
  let e = Rho (RhoMono Int) in
  let expected = Mono Int in
  (* ACT *)
  let output = substType e 0 input in
  (* ASSERT *)
  assert_equal expected output

let applyTypeMonoInt _ =
  (* ARRANGE *)
  let input = Mono Int in
  let e = Mono Int in
  let expected = Mono Int in
  (* ACT *)
  let output = applyType e input in
  (* ASSERT *)
  assert_equal expected output

let getListTypeMono _ =
  (* ARRANGE *)
  let input = Mono (List Int) in
  let expected = Mono Int in
  (* ACT *)
  let output = getListType input in
  (* ASSERT *)
  assert_equal expected output

let getListTypeRho _ =
  (* ARRANGE *)
  let input = Rho (RhoList (1, RhoMono Int)) in
  let expected = Poly (1, RhoMono Int) in
  (* ACT *)
  let output = getListType input in
  (* ASSERT *)
  assert_equal expected output

let getListTypeException _ =
  (* ARRANGE *)
  let input = Mono Int in
  let expected = Failure "not a list" in
  (* ACT *)
  let output _ = getListType input in
  (* ASSERT *)
  assert_raises expected output

let typeGenreToListMono _ =
  (* ARRANGE *)
  let input = Mono Int in
  let expected = Mono (List Int) in
  (* ACT *)
  let output = typeGenreToList input in
  (* ASSERT *)
  assert_equal expected output

let typeGenreToListRho _ =
  (* ARRANGE *)
  let input = Rho (RhoFun ((1, RhoMono Int), (2, RhoMono (Var 0)))) in
  let expected =
    Rho (RhoList (0, RhoFun ((1, RhoMono Int), (2, RhoMono (Var 0)))))
  in
  (* ACT *)
  let output = typeGenreToList input in
  (* ASSERT *)
  assert_equal expected output

let typeGenreToListPoly _ =
  (* ARRANGE *)
  let input = Poly (1, RhoMono Int) in
  let expected = Rho (RhoList (1, RhoMono Int)) in
  (* ACT *)
  let output = typeGenreToList input in
  (* ASSERT *)
  assert_equal expected output

let main =
  "DBAstTest"
  >::: [
         "typeGenreToMonoBool" >:: typeGenreToMonoBool;
         "typeGenreToMonoRhoMonoInt" >:: typeGenreToMonoRhoMonoInt;
         "typeGenreToMonoPolyInt" >:: typeGenreToMonoPolyInt;
         "typeGenreToMonoPolyException" >:: typeGenreToMonoPolyException;
         "typeGenreToRhoPoly0" >:: typeGenreToRhoPoly0;
         "typeGenreToRhoException" >:: typeGenreToRhoException;
         "typeGenreToPoly" >:: typeGenreToPoly;
         "normalizeRhoListMonoInt" >:: normalizeRhoListMonoInt;
         "isMonoTypePairForAllIntIntExpectFalse"
         >:: isMonoTypePairForAllIntIntExpectFalse;
         "isMonoTypeList" >:: isMonoTypeList;
         "typeToDeBruijnRho" >:: typeToDeBruijnRho;
         "typeToDeBruijnRho2" >:: typeToDeBruijnRho2;
         "monoTypeToDeBruijnPairIntBool" >:: monoTypeToDeBruijnPairIntBool;
         "monoTypeToDeBruijnException" >:: monoTypeToDeBruijnException;
         "rhoTypeToDeBruijnPairIntForAll" >:: rhoTypeToDeBruijnPairIntForAll;
         "rhoTypeToDeBruijnList" >:: rhoTypeToDeBruijnList;
         "rhoTypeToDeBruijnException" >:: rhoTypeToDeBruijnException;
         "shiftMonoInt" >:: shiftMonoInt;
         "shiftMonoBool" >:: shiftMonoBool;
         "shiftMonoUnit" >:: shiftMonoUnit;
         "shiftMonoPair" >:: shiftMonoPair;
         "shiftMonoList" >:: shiftMonoList;
         "shiftRhoPair" >:: shiftRhoPair;
         "shiftRhoList" >:: shiftRhoList;
         "shiftTypePoly4Var2" >:: shiftTypePoly4Var2;
         "substMonoInt" >:: substMonoInt;
         "substMonoBool" >:: substMonoBool;
         "substMonoUnit" >:: substMonoUnit;
         "substMonoPair" >:: substMonoPair;
         "substMonoList" >:: substMonoList;
         "substRhoFun" >:: substRhoFun;
         "substRhoPair" >:: substRhoPair;
         "substRhoList" >:: substRhoList;
         "substPolyPoly" >:: substPolyPoly;
         "substPolyRho" >:: substPolyRho;
         "substTypePoly" >:: substTypePoly;
         "substTypeRho" >:: substTypeRho;
         "applyTypeMonoInt" >:: applyTypeMonoInt;
         "getListTypeMono" >:: getListTypeMono;
         "getListTypeRho" >:: getListTypeRho;
         "getListTypeException" >:: getListTypeException;
         "typeGenreToListMono" >:: typeGenreToListMono;
         "typeGenreToListRho" >:: typeGenreToListRho;
         "typeGenreToListPoly" >:: typeGenreToListPoly;
       ]

let intToString _ =
  (* ARRANGE *)
  let input = Mono Int in
  let expected = "int" in
  (* ACT *)
  let output = typeGenreToString' 16 input in
  (* ASSERT *)
  assert_equal expected output

let varToString _ =
  (* ARRANGE *)
  let input = Mono (Var 0) in
  let expected = "p" in
  (* ACT *)
  let output = typeGenreToString' 16 input in
  (* ASSERT *)
  assert_equal expected output

let boolToString _ =
  (* ARRANGE *)
  let input = Mono Bool in
  let expected = "bool" in
  (* ACT *)
  let output = typeGenreToString' 16 input in
  (* ASSERT *)
  assert_equal expected output

let unitToString _ =
  (* ARRANGE *)
  let input = Mono Unit in
  let expected = "unit" in
  (* ACT *)
  let output = typeGenreToString' 16 input in
  (* ASSERT *)
  assert_equal expected output

let pairToString _ =
  (* ARRANGE *)
  let input = Mono (Pair (Int, Bool)) in
  let expected = "(int, bool)" in
  (* ACT *)
  let output = typeGenreToString' 16 input in
  (* ASSERT *)
  assert_equal expected output

let funToString _ =
  (* ARRANGE *)
  let input = Mono (Fun (Int, Int)) in
  let expected = "int -> int" in
  (* ACT *)
  let output = typeGenreToString' 16 input in
  (* ASSERT *)
  assert_equal expected output

let funToString2 _ =
  (* ARRANGE *)
  let input = Mono (Fun (Fun (Bool, Int), Int)) in
  let expected = "(bool -> int) -> int" in
  (* ACT *)
  let output = typeGenreToString' 16 input in
  (* ASSERT *)
  assert_equal expected output

let forAllToString _ =
  (* ARRANGE *)
  let input = Poly (1, RhoMono (Var 0)) in
  let expected = "forall a1. (a1)" in
  (* ACT *)
  let output = typeGenreToString' 16 input in
  (* ASSERT *)
  assert_equal expected output

let listToString _ =
  (* ARRANGE *)
  let input = Mono (List (Pair (Int, Bool))) in
  let expected = "(int, bool) list" in
  (* ACT *)
  let output = typeGenreToString' 16 input in
  (* ASSERT *)
  assert_equal expected output

let rhoPairToString _ =
  (* ARRANGE *)
  let input = Poly (2, RhoPair ((1, RhoMono (Var 0)), (2, RhoMono (Var 3)))) in
  let expected = "forall a1 b1. ((forall c1. (c1), forall c1 d1. (a1)))" in
  (* ACT *)
  let output = typeGenreToString' 16 input in
  (* ASSERT *)
  assert_equal expected output

let rhoFunToString _ =
  (* ARRANGE *)
  let input = Poly (2, RhoFun ((1, RhoMono (Var 0)), (2, RhoMono (Var 3)))) in
  let expected = "forall a1 b1. (forall c1. (c1) -> forall c1 d1. (a1))" in
  (* ACT *)
  let output = typeGenreToString' 16 input in
  (* ASSERT *)
  assert_equal expected output

let rhoMonoToString _ =
  (* ARRANGE *)
  let input = Rho (RhoMono Int) in
  let expected = "int" in
  (* ACT *)
  let output = typeGenreToString' 16 input in
  (* ASSERT *)
  assert_equal expected output

let rhoListToString _ =
  (* ARRANGE *)
  let input = Rho (RhoList (0, RhoMono Int)) in
  let expected = "int list" in
  (* ACT *)
  let output = typeGenreToString' 16 input in
  (* ASSERT *)
  assert_equal expected output

let suiteToString =
  "DBAstToStringTest"
  >::: [
         "intToString" >:: intToString;
         "varToString" >:: varToString;
         "boolToString" >:: boolToString;
         "unitToString" >:: unitToString;
         "pairToString" >:: pairToString;
         "funToString" >:: funToString;
         "funToString2" >:: funToString2;
         "forAllToString" >:: forAllToString;
         "listToString" >:: listToString;
         "rhoPairToString" >:: rhoPairToString;
         "rhoFunToString" >:: rhoFunToString;
         "rhoMonoToString" >:: rhoMonoToString;
         "rhoListToString" >:: rhoListToString;
       ]

let () = run_test_tt_main main
let () = run_test_tt_main suiteToString
