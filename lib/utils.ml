let getFirst (a, _) = a
let getSecond (_, b) = b
let ( $ ) f g x = f (g x)

let getPositionString (position : Lexing.position) =
  Printf.sprintf "File \"%s\", line %d, character %d:" position.pos_fname
    position.pos_lnum
    (position.pos_cnum - position.pos_bol)

let lexerErrorMessage (line : Lexing.lexbuf) msg =
  let position = line.lex_curr_p in
  Printf.sprintf "%s\n%s" (getPositionString position) msg

exception LexBufException of string

(** Transforms Lexing buffer into a list of Abstract Syntax Trees*)
let lexbufToExprList line =
  try Parser.start Lexer.read line with
  | Lexer.SyntaxError m ->
      let msg = lexerErrorMessage line m in
      raise (LexBufException msg)
  | Parser.Error ->
      let msg = lexerErrorMessage line "Syntax error" in
      raise (LexBufException msg)

let getTypeAndReducedFromLexBuf (lexbuf : Lexing.lexbuf) (system : Infer.system)
    : (DBType.typeGenre * DBAst.expr) list =
  let astList = lexbufToExprList lexbuf in
  (* convert to debruijn *)
  let dBAst = List.map DBAst.toDeBruijn astList in
  (* infer type *)
  let infer = List.map (getSecond $ Infer.inferType system) dBAst in
  (* reduce expressions *)
  let reduced = List.map Simple.evaluate dBAst in
  (* combine result *)
  List.combine infer reduced

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
  let dBAstString = DBAst.exprListToString dBExprList in
  Printf.printf "DeBruijn:\n%s\n" dBAstString

let printTypeExpr typeExpr =
  let typeExprString = DBType.monoTypeToString typeExpr in
  Printf.printf "Type:\n%s\n" typeExprString

let printTypeGenre typeGenre =
  let typeGenreString = DBType.typeGenreToString typeGenre in
  Printf.printf "TypeGenre:\n%s\n" typeGenreString

let printSubst subst = Printf.printf "%s\n" (Subst.substToString subst)

let printCtx ctx =
  Printf.printf "[";
  let _ =
    List.map (fun tk -> Printf.printf "%s, " (DBType.typeGenreToString tk)) ctx
  in
  ();
  Printf.printf "]\n"

let printTypeAndTypeGenre (typ, expr) =
  Printf.printf "- : %s | %s"
    (DBType.typeGenreToString typ)
    (DBAst.exprToString expr)
