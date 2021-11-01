{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = pos.pos_cnum;
               pos_lnum = pos.pos_lnum + 1
    }
}

let int = '-'? ['0'-'9']+
let white = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"
let ident_reg_exp = ['A'-'Z' 'a'-'z']+ ['0'-'9' 'A'-'Z' 'a'-'z' '_' '\'']*

rule read =
  parse
  | white    { read lexbuf }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | "fn"     { FUN }
  | "=>"     { ARRVAL }
  | ';'      { SEMICOLON }
  | ','      { COMMA }
  | '('      { LPAR }
  | ')'      { RPAR }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | ident_reg_exp {VAR (Lexing.lexeme lexbuf)}
  | newline  { next_line lexbuf; read lexbuf }
  | eof      { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }