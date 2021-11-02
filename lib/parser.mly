%token <int> INT
%token <string> VAR
%token TRUE FALSE
%token EOF LPAR RPAR COMMA SEMICOLON FUN ARRVAL

%type <Ast.expr> appExpr
%type <Ast.expr> expr
%type <Ast.expr list> block

%start <Ast.expr list> start 

%%

start:
  | b = block EOF                        {b}

basicExpr:
  | i = INT                              {Ast.Int(i)}
  | v = VAR                              {Ast.Var(v)}
  | TRUE                                 {Ast.Bool(true)}
  | FALSE                                {Ast.Bool(false)}
  | LPAR RPAR                            {Ast.Unit}
  | LPAR e = expr RPAR                   {e}
  | LPAR e1 = expr COMMA e2 = expr RPAR  {Ast.Pair(e1, e2)}

expr:
  | b = basicExpr                        {b}
  | a = appExpr                          {a}
  | FUN v = VAR ARRVAL e = expr          {Ast.Fun(v, e)}

appExpr:
  | b1 = basicExpr b2 = basicExpr        {Ast.App(b1, b2)}
  | a = appExpr b = basicExpr            {Ast.App(a, b)}

block:
  | SEMICOLON b = block                  {b}
  | e = expr SEMICOLON b = block         {e :: b}
  | e = expr                             {[e]}
  |                                      {[]}
