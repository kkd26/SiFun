open Utils

let runFromFile filename =
  let inx = open_in filename in
  try
    (* lexical analyzer buffer from file *)
    let lexbuf = Lexing.from_channel inx in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    let typeAndReduced = getTypeAndReducedFromLexBuf lexbuf in
    List.iter (print_newline $ printTypeAndTypeGenre) typeAndReduced;
    close_in inx
  with e ->
    Exception.handleExceptions e;
    close_in_noerr inx;
    exit (-1)

let repl () =
  (* lexical analyzer buffer from stdin *)
  let lexbuf = Lexing.from_channel stdin in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "console" };
  Printf.printf "SiFun interactive command line";
  flush stdout;
  while true do
    (try
       Printf.printf "\n> ";
       flush stdout;
       let typeAndReduced = getTypeAndReducedFromLexBuf lexbuf in
       let headTypeAndReduced = List.hd typeAndReduced in
       printTypeAndTypeGenre headTypeAndReduced
     with e -> Exception.handleExceptions e);
    flush stdout
  done
