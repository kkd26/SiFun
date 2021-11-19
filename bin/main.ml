open Sifun
open Utils
open Debruijn

let fromFile filename () =
  let inx = Core.In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= filename} ;
  let astList = lexbufToExprList lexbuf in
  printAst astList ;
  let dBAst = List.map toDeBruijn astList in
  printDBAst dBAst ;
  let reduced = List.map Simple.reduceAll dBAst in
  printDBAst reduced ;
  let infer = List.map Infer.inferType dBAst in
  let _, types = List.split infer in
  let _ = List.map printTypeExpr types in
  Core.In_channel.close inx

let () =
  let open Core in
  Command.basic_spec ~summary:"Parse from file"
    Command.Spec.(empty +> anon ("filename" %: string))
    fromFile
  |> Command.run
