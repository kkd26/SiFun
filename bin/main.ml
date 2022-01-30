open Sifun
open Utils
open Debruijn

let fromFile filename () =
  try
    let inx = Core.In_channel.create filename in
    let lexbuf = Lexing.from_channel inx in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    let astList = lexbufToExprList lexbuf in
    let dBAst = List.map toDeBruijn astList in
    let infer = List.map Infer.inferTypeHMV dBAst in
    List.iter
      (fun (_, typ) ->
        Utils.printTypeExpr typ;
        Printf.printf "\n")
      infer;
    Core.In_channel.close inx
  with Failure msg ->
    Printf.eprintf "%s\n" msg;
    exit (-1)

let () =
  let open Core in
  Command.basic_spec ~summary:"Parse from file"
    Command.Spec.(empty +> anon ("filename" %: string))
    fromFile
  |> Command.run
