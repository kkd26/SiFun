open Sifun
open Interpreter

let usage_msg = "sifun [-i] <file>"
let interactive = ref false
let input_file = ref ""
let anon_fun filename = input_file := filename
let speclist = [ ("-i", Arg.Set interactive, "Run file in interactive mode") ]

let () =
  Arg.parse speclist anon_fun usage_msg;
  runFromFile !input_file;
  if !interactive then repl ()
