open Core
open Sifun
open Utils

let fromFile filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= filename} ;
  let astList = lexbufToExprList lexbuf in
  printAst astList ; printDBAst astList ; In_channel.close inx

let () =
  Command.basic_spec ~summary:"Parse from file"
    Command.Spec.(empty +> anon ("filename" %: string))
    fromFile
  |> Command.run