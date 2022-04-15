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

let printTypeKind typeKind =
  let typeKindString = DBType.typeKindToString typeKind in
  Printf.printf "TypeKind:\n%s\n" typeKindString

let printSubst subst = Printf.printf "%s\n" (Subst.substToString subst)

let printCtx ctx =
  Printf.printf "[";
  let _ =
    List.map (fun tk -> Printf.printf "%s, " (DBType.typeKindToString tk)) ctx
  in
  ();
  Printf.printf "]\n"
