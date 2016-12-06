/* Ocamlyacc parser for StockQ */

%{
open Ast
%}

%token INT FLOAT BOOL STRING VOID NULL TRUE FALSE
%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA DOT
%token AND NOT OR PLUS MINUS TIMES DIVIDE ASSIGN MODULO
%token PLUSEQ MINUSEQ TIMESEQ DIVIDEEQ MODULOEQ
%token EQ NEQ LT LEQ GT GEQ BAR
%token IF ELSE FOR WHILE RETURN RETURNS
%token STOCK ORDER PORTFOLIO
%token ARRAY STRUCT
%token FUNCTION
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS PLUSEQ MINUSEQ
%left TIMES DIVIDE MODULO TIMESEQ DIVIDEEQ MODULOEQ
%right NOT NEG
%right DOT

%start program
%type <Ast.program> program

%%

program:
        fdecls stmts EOF { $1, $2 }


fdecls:
    /* nothing */       { [] }
  | fdecl_list          { List.rev $1 }

fdecl_list:
    fdecl               { [$1] }
  | fdecl_list fdecl    { $2 :: $1 }


stmts:
    /* nothing */       { [] }
  | stmt_list           { List.rev $1 }

stmt_list:
    stmt                { [$1] }
  | stmt_list stmt      { $2 :: $1 }


fdecl:
    FUNCTION ID LPAREN formals_opt RPAREN RETURNS typ LBRACE stmt_list RBRACE
    { {
        fname = $2;
        formals = $4;
        typ = $7;
        body = List.rev $9
    } }

formals_opt:
    /* nothing */   { [] }
|   formal_list     { List.rev $1 }

formal_list:
    typ ID                      { [($1, $2)] }
|   formal_list COMMA typ ID    { ($3, $4) :: $1 }

typ:
    INT         { Int }
|   FLOAT       { Float }
|   BOOL        { Bool }
|   VOID        { Void }
|   STOCK       { Stock }
|   ORDER       { Order }
|   PORTFOLIO   { Portfolio }
|   STRING      { String }
|   ARRAY       { Array }
|   STRUCT      { Struct }


stmt:
    expr SEMI                       { Expr $1 }
|   RETURN SEMI                     { Return Noexpr }
|   RETURN expr SEMI                { Return $2 }
|   LBRACE stmt_list RBRACE         { Block(List.rev $2) }
|   IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
|   IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
|   FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
        { For($3, $5, $7, $9) }
|   WHILE LPAREN expr RPAREN stmt   { While($3, $5) }
|   typ ID SEMI                     { Local($1, $2, Noexpr) }
|   typ ID ASSIGN expr SEMI         { Local($1, $2, $4) }

expr_opt:
    /* nothing */   { Noexpr }
|   expr            { $1 }

expr:
    INT_LITERAL         { IntLiteral($1) }
|   FLOAT_LITERAL       { FloatLiteral($1) }
|   STRING_LITERAL      { StringLiteral($1) }
|   TRUE                { BoolLiteral(true) }
|   FALSE               { BoolLiteral(false) }
|   ID                  { Id($1) }
|   expr PLUS   expr    { Binop($1, Add, $3) }
|   expr MINUS  expr    { Binop($1, Sub, $3) }
|   expr TIMES  expr    { Binop($1, Mult, $3) }
|   expr DIVIDE expr    { Binop($1, Div, $3) }
|   expr MODULO expr    { Binop($1, Mod, $3) }
|   expr EQ     expr    { Binop($1, Equal, $3) }
|   expr NEQ    expr    { Binop($1, Neq, $3) }
|   expr LT     expr    { Binop($1, Less, $3) }
|   expr LEQ    expr    { Binop($1, Leq, $3) }
|   expr GT     expr    { Binop($1, Greater, $3) }
|   expr GEQ    expr    { Binop($1, Geq, $3) }
|   expr AND    expr    { Binop($1, And, $3) }
|   expr OR     expr    { Binop($1, Or, $3) }
|   NOT expr            { Unop (Not, $2) }
|   expr DOT    expr    { ObjAccess($1, $3) }
|   MINUS expr          { Unop(Neg, $2) }
|   ID ASSIGN   expr    { Assign($1, $3) }
|   ID LPAREN actuals_opt RPAREN    { Call($1, $3) }
|   LPAREN expr RPAREN  { $2 }


actuals_opt:
            /* nothing */   { [] }
    |       actuals_list    { List.rev $1 }

actuals_list:
            expr                    { [$1] }
    |       actuals_list COMMA expr { $3 :: $1 }
