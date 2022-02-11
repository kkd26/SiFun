open Sifun
open Printf

let repl () =
  (* lexical analyzer buffer from stdin *)
  let lexbuf = Lexing.from_channel stdin in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "console" };
  printf "SiFun interactive command line";
  flush stdout;
  while true do
    (try
       printf "\n> ";
       flush stdout;
       (* read and parse an ast from the input *)
       let exp = List.hd (Utils.lexbufToExprList lexbuf) in
       (* convert to debruijn *)
       let debruijn = Debruijn.toDeBruijn exp in
       (* infer type *)
       let typ = snd (Infer.inferTypeHMV debruijn) in
       (* reduce expression *)
       let reduced = Simple.reduceAll debruijn in
       (* print the result *)
       printf "- : %s | %s"
         (DBType.typeKindToString typ)
         (Debruijn.exprToString reduced)
     with e -> Exception.handleExceptions e);
    flush stdout
  done

let () = repl ()
