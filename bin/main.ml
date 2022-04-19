open Sifun
open Interpreter

let usage_msg = "sifun [-i] <file>"
let interactive = ref false
let hm = ref false
let hmv = ref false
let input_file = ref ""
let anon_fun filename = input_file := filename

let speclist =
  [
    ("-i", Arg.Set interactive, "Run file in interactive mode");
    ("-hm", Arg.Set hm, "Run with HM type system");
    ("-hmv", Arg.Set hmv, "Run with HMV type system");
  ]

let getSystem () =
  if !hmv then Infer.HMV else if !hm then Infer.HM else Infer.BD

let () =
  Arg.parse speclist anon_fun usage_msg;
  let system = getSystem () in
  runFromFile system !input_file;
  if !interactive then repl system ()
