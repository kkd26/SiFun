(** Transforms Lexing buffer into a list of Abstract Syntax Trees*)
let lexbufToExprList line =
  try Parser.start Lexer.read line with
  | Lexer.SyntaxError msg -> failwith msg
  | Parser.Error ->
      let msg =
        Printf.sprintf "At offset %d: syntax error.\n%!"
          (Lexing.lexeme_start line) in
      failwith msg

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
