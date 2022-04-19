open Sifun
open Interpreter

let usage_msg = "sifuni [-hm -hmv]"

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  let system = getSystem () in
  repl system ()
