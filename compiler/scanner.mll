(* Ocamllex scanner for StockQ *)

{
    open Parser
    let lineno = ref 1
}

(* need to exclude newline for single line comments *)
let whitespace = [' ' '\t' '\r' '\n']
let alpha = ['a'-'z' 'A'-'Z']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let digit = ['0'-'9']
let int = digit+
let float = (digit*) ['.'] digit+
let char = ''' ( ascii | digit ) '''
let string = '"' ((ascii)* as s) '"'
let id = alpha (alpha | digit | '_')*


rule token = parse

(* white space *)
| whitespace    { token lexbuf }

(* comments *)
| "/*"          { comment lexbuf }
| "//"          { line_comment lexbuf }

(* symbols *)
| '('       { LPAREN }
| ')'       { RPAREN }
| '{'       { LBRACE }
| '}'       { RBRACE }
| '['       { LBRACKET }
| ']'       { RBRACKET }
| ';'       { SEMI }
| ','       { COMMA }

(* operators *)
| '+'       { PLUS }
| '-'       { MINUS }
| '*'       { TIMES }
| '/'       { DIVIDE }
| '%'       { MODULO }
| "+="      { PLUSEQ }
| "-="      { MINUSEQ }
| "*="      { TIMESEQ }
| "/="      { DIVIDEEQ }
| "%="      { MODULOEQ }
| '='       { ASSIGN }
| "=="      { EQ }
| "!="      { NEQ }
| '<'       { LT }
| "<="      { LEQ }
| '>'       { GT }
| ">="      { GEQ }
| "and"     { AND }
| "or"      { OR }
| "not"     { NOT }
| "true"    { TRUE }
| "false"   { FALSE }

(* branch control *)
| "if"      { IF }
| "else"    { ELSE }
| "for"     { FOR }
| "while"   { WHILE }

(* function operation *)
| "return"  { RETURN }
| "def"     { DEF }

(* primitives *)
| "int"     { INT }
| "float"   { FLOAT }
| "bool"    { BOOL }
| "string"  { STRING }
| "void"    { VOID }

(* data types *)
| "struct"  { STRUCT }
| "array"   { ARRAY }

(* literals *)
| int as lxm        { INT_LITERAL(int_of_string lxm) }
| float as lxm      { FLOAT_LITERAL(float_of_string lxm) }
| id as lxm         { ID(lxm) }
| string            { STRING_LITERAL(s) }
| eof               { EOF }
| _ as char         { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
    "*/"        { token lexbuf }
|   _           { comment lexbuf }

and line_comment = parse
    ['\n' '\r'] { token lexbuf }
|   _           { line_comment lexbuf }
