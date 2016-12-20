/* Ocamlyacc parser for StockQ */

%{
open Ast
%}

%token INT FLOAT BOOL STRING VOID TRUE FALSE
%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA
%token AND NOT OR PLUS MINUS TIMES DIVIDE ASSIGN MODULO
%token PLUSEQ MINUSEQ TIMESEQ DIVIDEEQ MODULOEQ
%token EQ NEQ LT LEQ GT GEQ
%token IF ELSE FOR WHILE RETURN DEF
%token ARRAY STRUCT
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

%start program
%type <Ast.program> program

%%

program:
    decls EOF { $1 }

decls:
    /* nothing */   { [], [] }
  | decls fdecl     { (fst $1 @ [$2]), snd $1 }
  | decls stmt      { fst $1, (snd $1 @ [$2]) }

stmt_list:
    stmt                { [$1] }
  | stmt_list stmt      { $2 :: $1 }


fdecl:
    DEF datatype ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
    { {
        ftype = $2;
        fname = $3;
        formals = $5;
        body = List.rev $8
    } }

formals_opt:
    /* nothing */   { [] }
|   formal_list     { List.rev $1 }

formal_list:
    datatype ID                     { [($1, $2)] }
|   formal_list COMMA datatype ID   { ($3, $4) :: $1 }

datatype:
  | primitive   { Primitive($1) }
  | array_type  { $1 }

array_type:
    primitive LBRACKET brackets RBRACKET { Arraytype($1, $3) }

brackets:
    /* nothing */               { 1 }
  | brackets RBRACKET LBRACKET  { $1 + 1 }

primitive:
    INT         { Int }
|   FLOAT       { Float }
|   BOOL        { Bool }
|   STRING      { String }
|   VOID        { Void }
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
|   datatype ID SEMI                     { Local($1, $2, Noexpr) }
|   datatype ID ASSIGN expr SEMI         { Local($1, $2, $4) }

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
|   expr PLUSEQ expr    { Binop($1, Addeq, $3) }
|   expr MINUSEQ  expr  { Binop($1, Subeq, $3) }
|   expr TIMESEQ  expr  { Binop($1, Multeq, $3) }
|   expr DIVIDEEQ expr  { Binop($1, Diveq, $3) }
|   expr MODULOEQ expr  { Binop($1, Modeq, $3) }
|   expr EQ     expr    { Binop($1, Equal, $3) }
|   expr NEQ    expr    { Binop($1, Neq, $3) }
|   expr LT     expr    { Binop($1, Less, $3) }
|   expr LEQ    expr    { Binop($1, Leq, $3) }
|   expr GT     expr    { Binop($1, Greater, $3) }
|   expr GEQ    expr    { Binop($1, Geq, $3) }
|   expr AND    expr    { Binop($1, And, $3) }
|   expr OR     expr    { Binop($1, Or, $3) }
|   NOT expr            { Unop (Not, $2) }
|   MINUS expr          { Unop(Neg, $2) }
|   ID ASSIGN   expr    { Assign($1, $3) }
|   expr ASSIGN expr    { ArrayAssign($1, $3) }
|   ID LPAREN actuals_opt RPAREN    { Call($1, $3) }
|   primitive bracket_args RBRACKET { ArrayCreate(Primitive($1), List.rev $2) }
|   expr bracket_args RBRACKET      { ArrayAccess($1, List.rev $2) }
|   LPAREN expr RPAREN  { $2 }

bracket_args:
    |       LBRACKET expr                       { [$2] }
    |       bracket_args RBRACKET LBRACKET expr { $4 :: $1 }

actuals_opt:
            /* nothing */   { [] }
    |       actuals_list    { List.rev $1 }

actuals_list:
            expr                    { [$1] }
    |       actuals_list COMMA expr { $3 :: $1 }
