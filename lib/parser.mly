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

appExpr:
  | i = INT                              {Ast.Int(i)}
  | v = VAR                              {Ast.Var(v)}
  | TRUE                                 {Ast.Bool(true)}
  | FALSE                                {Ast.Bool(false)}
  | LPAR RPAR                            {Ast.Unit}
  | LPAR e = expr RPAR                   {e}
  | LPAR e1 = expr COMMA e2 = expr RPAR  {Ast.Pair(e1, e2)}  

expr:
  | a = appExpr                          {a}
  | e = expr a = appExpr                 {Ast.App(e, a)}
  | FUN v = VAR ARRVAL e = appExpr       {Ast.Fun(v, e)}

block:
  | e = expr                             {[e]}
  | e = expr SEMICOLON                   {[e]}
  | e = expr SEMICOLON b = block         {e :: b}
