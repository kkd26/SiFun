open Sifun
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

let headToString _ =
  (* ARRANGE *)
  let input = Head (List [ Int 1; Unit ]) in
  let expected = "Head(List(Int(1)\nUnit\n))" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let tailToString _ =
  (* ARRANGE *)
  let input = Tail (List [ Int 1; Unit ]) in
  let expected = "Tail(List(Int(1)\nUnit\n))" in
  (* ACT *)
  let output = exprToString input in
  (* ASSERT *)
  assert_equal expected output

let exprListToString _ =
  (* ARRANGE *)
  let input = [ Int 1; Unit; Var "p" ] in
  let expected = "Int(1)\nUnit\nVar(p)\n" in
  (* ACT *)
  let output = exprListToString input in
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
         "headToString" >:: headToString;
         "tailToString" >:: tailToString;
         "exprListToString" >:: exprListToString;
       ]

let () = run_test_tt_main suiteToString
