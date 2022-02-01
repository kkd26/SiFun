let getPositionString (position : Lexing.position) =
  Printf.sprintf "File \"%s\", line %d, character %d:" position.pos_fname
    position.pos_lnum
    (position.pos_cnum - position.pos_bol)

let lexerErrorMessage (line : Lexing.lexbuf) msg =
  let position = line.lex_curr_p in
  Printf.sprintf "%s\n%s" (getPositionString position) msg

exception LexBufError of string

(** Transforms Lexing buffer into a list of Abstract Syntax Trees*)
let lexbufToExprList line =
  try Parser.start Lexer.read line with
  | Lexer.SyntaxError m ->
      let msg = lexerErrorMessage line m in
      raise (LexBufError msg)
  | Parser.Error ->
      let msg = lexerErrorMessage line "Syntax error" in
      raise (LexBufError msg)

(** Parses a string into a list of AST *)
let stringToExprList input =
  let lexbuf = Lexing.from_string input in
  lexbufToExprList lexbuf

(** Prints formatted AST on the screen*)
let printAst astList =
  let astString = Ast.exprListToString astList in
  Printf.printf "AST:\n%s\n" astString

(** Prints formatted AST after transforming it into the de Bruijn indices *)
let printDBAst dBExprList =
  let dBAstString = Debruijn.exprListToString dBExprList in
  Printf.printf "DeBruijn:\n%s\n" dBAstString

let printTypeExpr typeExpr =
  let typeExprString = DBType.typeExprToString typeExpr in
  Printf.printf "Type:\n%s\n" typeExprString

let printSubst subst = Printf.printf "%s\n" (Subst.substToString subst)
