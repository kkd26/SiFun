open Sifun
open Interpreter

let usage_msg = "sifun [-i -hm -hmv] <filename>"
let input_file = ref ""
let anon_fun filename = input_file := filename

let () =
  Arg.parse speclist anon_fun usage_msg;
  let system = getSystem () in
  runFromFile system !input_file;
  if !interactive then repl system ()
