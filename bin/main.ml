open Sifun
open Interpreter

let usage_msg = "sifun [-i -hm -hmv] [-e <string>] <filename>"
let input_file = ref ""
let anon_fun filename = input_file := filename

let () =
  Arg.parse speclist anon_fun usage_msg;
  let system = getSystem () in
  let printMode = getPrintMode () in
  if !eval_string <> "" then runFromString system printMode !eval_string
  else runFromFile system printMode !input_file;
  if !interactive then repl system printMode ()
