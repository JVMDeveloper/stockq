open Ast

let string_of_op = function
  | Add     -> "Add"
  | Sub     -> "Sub"
  | Mult    -> "Mult"
  | Div     -> "Div"
  | Mod     -> "Mod"
  | Equal   -> "Equal"
  | Neq     -> "Neq"
  | Less    -> "Less"
  | Leq     -> "Leq"
  | Greater -> "Greater"
  | Geq     -> "Geq"
  | And     -> "And"
  | Or      -> "Or"

let string_of_uop = function
  | Neg -> "Neg"
  | Not -> "Not"

let string_of_typ = function
  | Int     -> "int"
  | Float   -> "float"
  | Bool    -> "bool"
  | Void    -> "void"
  | Stock   -> "stock"
  | Order   -> "order"
  | Portfolio -> "portfolio"
  | String  -> "string"
  | Array   -> "array"
  | Struct  -> "struct"

let rec string_of_expr = function
  | IntLiteral(i)       -> "IntLiteral(" ^ string_of_int i ^ ")"
  | FloatLiteral(f)     -> "FloatLiteral(" ^ string_of_float f ^ ")"
  | StringLiteral(s)    -> "StringLiteral(" ^ s ^ ")"
  | BoolLiteral(b)      -> "BoolLiteral(" ^ string_of_bool b ^ ")"
  | Id(s)               -> "Id(" ^ s ^ ")"
  | Binop(e1, op, e2)   ->
      let v1 = string_of_expr e1
      and v2 = string_of_expr e2
      and oper = string_of_op op in
      "Binop(" ^ v1 ^ ", " ^ oper ^ ", " ^ v2 ^ ")"
  | Unop(uop, e)        -> "Unop(" ^ string_of_uop uop ^ ", " ^
                            string_of_expr e ^ ")"
  | Assign(s, e)        -> "Assign(" ^ s ^ ", " ^ string_of_expr e ^ ")"
  | Call(s, el)         -> "Call(" ^ s ^ ", " ^
                           String.concat ", " (List.map string_of_expr el) ^
                           ")"
  | ObjAccess(e1, e2)   -> "ObjAccess(" ^
                           string_of_expr e1 ^ ", " ^
                           string_of_expr e2 ^ ")"
  | Noexpr              -> "Noexpr"

let rec string_of_stmt = function
  | Block(sl)       ->  "Block(" ^
                        String.concat ", " (List.map string_of_stmt sl) ^ ")"
  | Expr(e)         ->  string_of_expr e
  | Return(e)       ->  "Return(" ^ string_of_expr e ^ ")"
  | If(e, s1, s2)   ->  "If(" ^ string_of_expr e ^ ") { " ^ string_of_stmt s1 ^
                        " } Else { " ^ string_of_stmt s2 ^ " }"
  | For(e1, e2, e3, s)  ->  "For(" ^
                            string_of_expr e1 ^ "; " ^
                            string_of_expr e2 ^ "; " ^
                            string_of_expr e3 ^ ") { " ^
                            string_of_stmt s ^ " }"
  | While(e, s)     ->  "While(" ^ string_of_expr e ^ ") { " ^
                        string_of_stmt s ^ " }"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let rec string_of_func func =
  "FUNCTION " ^ func.fname ^ " (" ^
  String.concat ", " (List.map snd func.formals) ^ ") " ^
  "returns " ^ string_of_typ func.typ ^ "\n{\n\t" ^
  String.concat "\n\t" (List.map string_of_stmt func.body) ^
  "\n}\n"

let rec string_of_program stor = function
  | ([], [])         -> String.concat "\n" (List.rev stor)
  | ([], stmt :: tl) -> string_of_program (string_of_stmt stmt :: stor) ([], tl)
  | (func :: tl, []) -> string_of_program (string_of_func func :: stor) (tl, [])

    (* print all functions first, then statements *)
  | (func :: ftl, stmts) ->
        string_of_program (string_of_func func :: stor) (ftl, stmts)

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.token lexbuf in
  let result = string_of_program [] ast in
  print_endline result
