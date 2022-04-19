open Utils

let interactive = ref false
let hm = ref false
let hmv = ref false

let speclist =
  [
    ("-i", Arg.Set interactive, "Run in interactive mode");
    ("-hm", Arg.Set hm, "Run with HM type system");
    ("-hmv", Arg.Set hmv, "Run with HMV type system");
  ]

let getSystem () =
  if !hmv then Infer.HMV else if !hm then Infer.HM else Infer.BD

let runFromFile system filename =
  let inx = open_in filename in
  try
    (* lexical analyzer buffer from file *)
    let lexbuf = Lexing.from_channel inx in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    let typeAndReduced = getTypeAndReducedFromLexBuf lexbuf system in
    List.iter (print_newline $ printTypeAndTypeGenre) typeAndReduced;
    close_in inx
  with e ->
    Exception.handleExceptions e;
    close_in_noerr inx;
    exit (-1)

let repl system () =
  (* lexical analyzer buffer from stdin *)
  let lexbuf = Lexing.from_channel stdin in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "console" };
  Printf.printf "SiFun interactive command line";
  flush stdout;
  while true do
    (try
       Printf.printf "\n> ";
       flush stdout;
       let typeAndReduced = getTypeAndReducedFromLexBuf lexbuf system in
       let headTypeAndReduced = List.hd typeAndReduced in
       printTypeAndTypeGenre headTypeAndReduced
     with e -> Exception.handleExceptions e);
    flush stdout
  done
