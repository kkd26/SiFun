%token <int> INT
%token <string> VAR
%token TRUE FALSE
%token FST SND
%token EOF LPAR RPAR COMMA SEMICOLON FUN ARRVAL
%token TINT TBOOL TUNIT TFORALL TARR DOT LCUR RCUR LAM COLON

%type <Ast.expr> basicExpr
%type <Ast.expr> appExpr
%type <Type.monoType> basicTypeExpr
%type <Type.monoType> funTypeExpr
%type <Type.monoType> typeExpr
%type <Ast.expr> expr
%type <Ast.expr list> block

%start <Ast.expr list> start 

%%

start:
  | b = block EOF                           {b}

block:
  | SEMICOLON b = block                     {b}
  | e = expr SEMICOLON b = block            {e :: b}
  | e = expr                                {[e]}
  |                                         {[]}

expr:
  | b = basicExpr                           {b}
  | a = appExpr                             {a}
  | FUN v = VAR ARRVAL e = expr             {Ast.Fun(v, e)}
  | FUN v = VAR COLON t = typeExpr ARRVAL e = expr {Ast.FunType(v, t, e)}
  
  | LAM l = varList DOT e = expr            {Ast.bigLamFromList l e}

basicExpr:
  | i = INT                                 {Ast.Int(i)}
  | v = VAR                                 {Ast.Var(v)}
  | TRUE                                    {Ast.Bool(true)}
  | FALSE                                   {Ast.Bool(false)}
  | LPAR RPAR                               {Ast.Unit}
  | LPAR e = expr RPAR                      {e}
  | LPAR e1 = expr COMMA e2 = expr RPAR     {Ast.Pair(e1, e2)}
  | FST b = basicExpr                       {Ast.Fst(b)}
  | SND b = basicExpr                       {Ast.Snd(b)}

appExpr:
  | b1 = basicExpr b2 = basicExpr           {Ast.App(b1, b2)}
  | a = appExpr b = basicExpr               {Ast.App(a, b)}
  | b = basicExpr LCUR t = typeExpr RCUR    {Ast.FunApp(b, t)}

typeExpr:
  | f = funTypeExpr                         {f}
  | TFORALL l = varList DOT f = funTypeExpr {Type.forAllFromList l f}

funTypeExpr:
  | b = basicTypeExpr                       {b}
  | b = basicTypeExpr TARR t = typeExpr     {Type.Fun(b, t)}

basicTypeExpr:
  | v = VAR                                 {Type.Var(v)}
  | TINT                                    {Type.Int}
  | TBOOL                                   {Type.Bool}
  | TUNIT                                   {Type.Unit}
  | LPAR t = typeExpr RPAR                  {t}

varList:
  |                                         {[]}
  | v = VAR l = varList                     {v :: l}
