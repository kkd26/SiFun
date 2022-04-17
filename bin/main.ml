open Sifun
open Utils
open DBAst

let fromFile filename () =
  try
    let inx = Core.In_channel.create filename in
    let lexbuf = Lexing.from_channel inx in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    let astList = lexbufToExprList lexbuf in
    let dBAst = List.map toDeBruijn astList in
    let reduced = List.map Simple.evaluate dBAst in
    let infer = List.map Infer.inferTypeBD dBAst in
    let typeAndReduced = List.combine infer reduced in
    List.iter
      (fun ((_, typ), reduced) ->
        Printf.printf "- : %s | %s\n"
          (DBType.typeGenreToString typ)
          (DBAst.exprToString reduced))
      typeAndReduced;
    Core.In_channel.close inx
  with e ->
    Exception.handleExceptions e;
    exit (-1)

let () =
  let open Core in
  Command.basic_spec ~summary:"Parse from file"
    Command.Spec.(empty +> anon ("filename" %: string))
    fromFile
  |> Command.run
