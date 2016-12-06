open Parser

let stringify = function
  | LPAREN  -> "LPAREN" | RPAREN    -> "RPAREN"
  | LBRACE  -> "LBRACE" | RBRACE    -> "RBRACE"
  | SEMI    -> "SEMI"   | COMMA     -> "COMMA"

  | PLUS    -> "PLUS"   | MINUS     -> "MINUS"
  | TIMES   -> "TIMES"  | DIVIDE    -> "DIVIDE"
  | MODULO  -> "MODULO"

  | PLUSEQ  -> "PLUSEQ"     | MINUSEQ   -> "MINUSEQ"
  | TIMESEQ -> "TIMESEQ"    | DIVIDEEQ  -> "DIVIDEEQ"
  | MODULOEQ -> "MODULOEQ"

  | ASSIGN  -> "ASSIGN"

  | EQ      -> "EQ"     | NEQ       -> "NEQ"
  | LT      -> "LT"     | LEQ       -> "LEQ"
  | GT      -> "GT"     | GEQ       -> "GEQ"
  | AND     -> "AND"    | OR        -> "OR"
  | NOT     -> "NOT"

  | DOT         -> "DOT"        | BAR       -> "BAR"
  | LBRACKET    -> "LBRACKET"   | RBRACKET  -> "RBRACKET"

  | IF      -> "IF"
  | ELSE    -> "ELSE"
  | FOR     -> "FOR"
  | WHILE   -> "WHILE"
  | RETURN  -> "RETURN"
  | RETURNS -> "RETURNS"

  | INT     -> "INT"    | FLOAT     -> "FLOAT"
  | BOOL    -> "BOOL"   | VOID      -> "VOID"
  | TRUE    -> "TRUE"   | FALSE     -> "FALSE"
  | NULL    -> "NULL"   | ORDER     -> "ORDER"
  | STOCK   -> "STOCK"  | PORTFOLIO -> "PORTFOLIO"
  | STRUCT  -> "STRUCT" | ARRAY     -> "ARRAY"
  | STRING  -> "STRING"

  | FUNCTION -> "FUNCTION"

  | INT_LITERAL(i)      -> "INT_LITERAL"
  | FLOAT_LITERAL(f)    -> "FLOAT_LITERAL"
  | ID(id)              -> "ID"
  | STRING_LITERAL(str) -> "STRING_LITERAL"
  | EOF                 -> "EOF"

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let rec print_tokens = function
    | EOF -> " "
    | token ->
      print_endline (stringify token);
      print_tokens (Scanner.token lexbuf) in
  print_tokens (Scanner.token lexbuf)
