open Sifun
open OUnit2
open DBType
open Deepskolem
open State.IntState

let dskRhoFun _ =
  (* ARRANGE *)
  let input = Rho (RhoFun ((1, RhoMono Int), (2, RhoMono (Var 1)))) in
  let input2 = RhoFun ((1, RhoMono (Var 0)), (2, RhoMono Bool)) in
  let expected = [ (0, Mono Int); (1, Mono Bool) ] in
  (* ACT *)
  let output = snd (runState (dsk' input input2) ~init:0) in
  (* ASSERT *)
  assert_equal expected output

let dskRhoPair _ =
  (* ARRANGE *)
  let input = Rho (RhoPair ((1, RhoMono (Var 0)), (2, RhoMono (Var 1)))) in
  let input2 = RhoPair ((1, RhoMono Int), (2, RhoMono Bool)) in
  let expected = [ (0, Mono Int); (1, Mono Bool) ] in
  (* ACT *)
  let output = snd (runState (dsk' input input2) ~init:0) in
  (* ASSERT *)
  assert_equal expected output

let dskError _ =
  (* ARRANGE *)
  let input = Mono Int in
  let input2 = RhoFun ((1, RhoMono Int), (0, RhoMono Bool)) in
  let expected = Failure "skolem error" in
  (* ACT *)
  let output _ = snd (runState (dsk' input input2) ~init:0) in
  (* ASSERT *)
  assert_raises expected output

let main =
  "DeepskolemisationTest"
  >::: [
         "dskRhoFun" >:: dskRhoFun;
         "dskRhoPair" >:: dskRhoPair;
         "dskError" >:: dskError;
       ]

let () = run_test_tt_main main
