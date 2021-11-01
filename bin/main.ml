open Sifun
open Core

let process line =
  try
    (* Run the parser on this line of input. *)
    Printf.printf "%s\n%!" (Ast.printExpList (Parser.start Lexer.read line))
  with
  | Lexer.SyntaxError msg -> Printf.fprintf stderr "%s%!" msg
  | Parser.Error ->
      Printf.fprintf stderr "At offset %d: syntax error.\n%!"
        (Lexing.lexeme_start line)

let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= filename} ;
  process lexbuf ;
  In_channel.close inx

let () =
  Command.basic_spec ~summary:"Parse and display JSON"
    Command.Spec.(empty +> anon ("filename" %: string))
    loop
  |> Command.run
