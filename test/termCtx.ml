open Sifun
open TermCtx
open OUnit2

let emptyTermCtx _ =
  (* ARRANGE *)
  let input = emptyCtx in
  let k x = x in
  let expected = ContextException "Out of bounds variable 0" in
  (* ACT *)
  let output _ = input 0 k in
  (* ASSERT *)
  assert_raises expected output

let main = "TermContextTests" >::: [ "emptyTermCtx" >:: emptyTermCtx ]
let () = run_test_tt_main main
