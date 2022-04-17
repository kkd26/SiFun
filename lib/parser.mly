%token <int> INT
%token <string> VAR
%token TRUE FALSE
%token FST SND HD TL
%token EOF LPAR RPAR COMMA SEMICOLON FUN ARRVAL LBRA RBRA
%token TINT TBOOL TUNIT TFORALL TARR DOT LCUR RCUR LAM COLON

%type <Ast.expr list> block
%type <Ast.expr> expr
%type <Ast.expr> typeAppExpr
%type <Ast.expr> basicExpr
%type <Ast.expr> appExpr
%type <Type.monoType> basicTypeExpr
%type <Type.monoType> funTypeExpr
%type <Type.monoType> typeExpr

%start <Ast.expr list> start 

%%

start:
  | b = block EOF                                  {b}

block:
  | SEMICOLON b = block                            {b}
  | e = expr SEMICOLON b = block                   {e :: b}
  | e = expr                                       {[e]}
  |                                                {[]}

expr:
  | e = typeAppExpr                                {e}
  | a = appExpr                                    {a}
  | FUN v = VAR ARRVAL e = expr                    {Ast.Fun(v, e)}
  | FUN v = VAR COLON t = typeExpr ARRVAL e = expr {Ast.FunType(v, t, e)}
  | LAM l = varList DOT e = expr                   {Ast.bigLamFromList l e}

typeAppExpr: 
  | b = basicExpr                                  {b}
  | e = typeAppExpr LCUR t = typeExpr RCUR         {Ast.TypeApp(e, t)}
  | e = basicExpr COLON t = typeExpr               {Ast.Annot(e, t)}

basicExpr:
  | i = INT                                        {Ast.Int(i)}
  | v = VAR                                        {Ast.Var(v)}
  | TRUE                                           {Ast.Bool(true)}
  | FALSE                                          {Ast.Bool(false)}
  | LPAR RPAR                                      {Ast.Unit}
  | LPAR e = expr RPAR                             {e}
  | LPAR e1 = expr COMMA e2 = expr RPAR            {Ast.Pair(e1, e2)}
  | FST b = basicExpr                              {Ast.Fst(b)}
  | SND b = basicExpr                              {Ast.Snd(b)}
  | HD b = basicExpr                               {Ast.Head(b)}
  | TL b = basicExpr                               {Ast.Tail(b)}
  | LBRA b = block RBRA                            {Ast.List(b)}

appExpr:
  | e1 = typeAppExpr e2 = typeAppExpr              {Ast.App(e1, e2)}
  | a = appExpr e = typeAppExpr                    {Ast.App(a, e)}

typeExpr:
  | f = funTypeExpr                                {f}
  | TFORALL l = varList DOT f = funTypeExpr        {Type.forAllFromList l f}

funTypeExpr:
  | b = basicTypeExpr                              {b}
  | b = basicTypeExpr TARR t = typeExpr            {Type.Fun(b, t)}

basicTypeExpr:
  | v = VAR                                        {Type.Var(v)}
  | TINT                                           {Type.Int}
  | TBOOL                                          {Type.Bool}
  | TUNIT                                          {Type.Unit}
  | LPAR t = typeExpr RPAR                         {t}
  | LPAR t1 = typeExpr COMMA t2 = typeExpr RPAR    {Type.Pair(t1, t2)}

varList:
  |                                                {[]}
  | v = VAR l = varList                            {v :: l}
