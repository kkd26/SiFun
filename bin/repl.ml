open Sifun
open Interpreter

let usage_msg = "sifuni [-hm -hmv]"

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  let system = getSystem () in
  let printMode = getPrintMode () in
  repl system printMode ()
