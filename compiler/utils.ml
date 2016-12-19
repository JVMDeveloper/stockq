open Ast
open Parser

module StringMap = Map.Make(String)


let string_of_op = function
  | Add     -> "Add"
  | Sub     -> "Sub"
  | Mult    -> "Mult"
  | Div     -> "Div"
  | Mod     -> "Mod"
  | Addeq   -> "Addeq"
  | Subeq   -> "Subeq"
  | Multeq  -> "Multeq"
  | Diveq   -> "Diveq"
  | Modeq   -> "Modeq"
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

let string_of_primitive = function
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

let rec print_brackets = function
  | 1 -> "[]"
  | i -> "[]" ^ print_brackets (i - 1)

let string_of_datatype = function
  | Primitive(p)    -> string_of_primitive p
  | Arraytype(p, i) -> string_of_primitive p ^ print_brackets i

let string_of_id = function
  | Id(s)   -> s
  | _       -> ""

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
  | Local(dt, s, e) -> "Local(" ^ string_of_datatype dt ^ ", " ^ s ^ ", " ^
                        string_of_expr e ^ ")"

(* let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n" *)

let string_of_func func =
  "def " ^ string_of_datatype func.ftype ^ " " ^ func.fname ^ " (" ^
  String.concat ", " (List.map snd func.formals) ^ ") {\n\t" ^
  String.concat "\n\t" (List.map string_of_stmt func.body) ^
  "\n}"

let rec string_of_program stor = function
  | ([], [])         -> String.concat "\n" (List.rev stor) ^ "\n"
  | ([], stmt :: tl) -> string_of_program (string_of_stmt stmt :: stor) ([], tl)
  | (func :: tl, []) -> string_of_program (string_of_func func :: stor) (tl, [])

    (* return all functions first, then statements *)
  | (func :: ftl, stmts) ->
        string_of_program (string_of_func func :: stor) (ftl, stmts)

let string_of_token = function
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

  | LBRACKET    -> "LBRACKET"   | RBRACKET  -> "RBRACKET"

  | IF      -> "IF"
  | ELSE    -> "ELSE"
  | FOR     -> "FOR"
  | WHILE   -> "WHILE"
  | RETURN  -> "RETURN"
  | DEF     -> "DEF"

  | INT     -> "INT"    | FLOAT     -> "FLOAT"
  | BOOL    -> "BOOL"   | VOID      -> "VOID"
  | TRUE    -> "TRUE"   | FALSE     -> "FALSE"
  | STRUCT  -> "STRUCT" | ARRAY     -> "ARRAY"
  | STRING  -> "STRING"

  | INT_LITERAL _       -> "INT_LITERAL"
  | FLOAT_LITERAL _     -> "FLOAT_LITERAL"
  | ID _                -> "ID"
  | STRING_LITERAL _    -> "STRING_LITERAL"
  | EOF                 -> "EOF"

(* returns ast type of expr *)
let type_of_expr func all_funcs = 

  let check_assign lvaluet rvaluet err =
    if lvaluet == rvaluet then lvaluet else raise err
  in

  let prim_of_dt = function
    | Primitive(p)      -> p
    | Arraytype(p, _)   -> p
  in

  let print_type = Primitive(Int) in
  let built_in_decls = StringMap.singleton "print"
    { ftype = Primitive(Void);
      fname = "print";
      formals = [(print_type, "x")];
      body = [] }
  in

  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                        built_in_decls all_funcs
  in

  let function_decl s = try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let locals =
    let rec get_locals mylocals = function
      | [] -> mylocals
      | [Local (t, s, _)] -> get_locals [(t, s)] []
      | Local (t, s, _) :: r -> get_locals ((t, s) :: mylocals) r
      | _ :: r -> get_locals mylocals r
    in
    get_locals [] func.body
  in

  (* Datatype of each variable (formal, or local *)
  let symbols =
    List.fold_left (fun m (t, n) -> StringMap.add n (prim_of_dt t) m)
      StringMap.empty (func.formals @ locals)
  in

  let type_of_identifier s =
    try StringMap.find s symbols
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in

  let rec expr = function
    | IntLiteral _    -> Int
    | FloatLiteral _  -> Float
    | BoolLiteral _   -> Bool
    | StringLiteral _ -> String
    | Id s            -> type_of_identifier s
    | Binop(e1, op, e2) as e ->
        let t1 = expr e1
        and t2 = expr e2 in
        (match op with
            | Add | Sub | Mult | Div | Mod when t1 = Int && t2 = Int -> Int
            | Add | Sub | Mult | Div | Mod when t1 = Int && t2 = Float -> Float
            | Add | Sub | Mult | Div | Mod when t1 = Float && t2 = Int -> Float
            | Add | Sub | Mult | Div | Mod when t1 = Float && t2 = Float -> Float

            | Addeq | Subeq | Multeq | Diveq | Modeq when t1 = Int && t2 = Int -> Int
            | Addeq | Subeq | Multeq | Diveq | Modeq when t1 = Int && t2 = Float -> Float
            | Addeq | Subeq | Multeq | Diveq | Modeq when t1 = Float && t2 = Int -> Float
            | Addeq | Subeq | Multeq | Diveq | Modeq when t1 = Float && t2 = Float -> Float
          
          | Equal | Neq when t1 = t2 -> Bool
          | Equal | Neq when (t1 = Int && t2 = Float) || (t1 = Float && t2 = Int) -> Bool
          | Less | Leq | Greater | Geq when t1 = t2 -> Bool
          | Less | Leq | Greater | Geq when (t1 = Int && t2 = Float) || (t1 = Float && t2 = Int) -> Bool
          | And | Or when t1 = Bool && t2 = Bool -> Bool
          | _ -> raise (Failure ("illegal binary operator " ^
              string_of_primitive t1 ^ " " ^ string_of_op op ^ " " ^
              string_of_primitive t2 ^ " in " ^ string_of_expr e))
        )
    | Unop(op, e) as ex -> let t = expr e in
        (match op with
          | Neg when t = Int -> Int
          | Neg when t = Float -> Float
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary op " ^ string_of_uop op ^
                  string_of_primitive t ^ " in " ^ string_of_expr ex))
        )
    | Assign(var, e) as ex ->
        let lt = type_of_identifier var
        and rt = expr e in
        check_assign lt rt (Failure ("illegal assignment " ^
          string_of_primitive lt ^ " = " ^ string_of_primitive rt ^
          " in " ^ string_of_expr ex))
    | Call(fname, actuals) as call ->
        if fname = "print" then
          if List.length actuals != 1 then
            raise (Failure ("expecting 1 argument in print")) 
          else
            let actual = List.hd actuals in
            let actual_t = expr actual in
            (match actual_t with
              | Int | Float | Bool | String -> actual_t
              | _ -> raise (Failure ("expecting int or float in print"))
            )
        else
          let fd = function_decl fname in
          if List.length actuals != List.length fd.formals then
            raise (Failure ("expecting " ^ string_of_int
              (List.length fd.formals) ^ " arguments in " ^
              string_of_expr call))
          else
            List.iter2 (fun (ft, _) e ->
              let ft = prim_of_dt ft in
              let et = expr e in
              ignore (check_assign ft et
                (Failure ("illegal actual argument found " ^
                  string_of_primitive et ^ " expected " ^
                  string_of_primitive et ^ " in " ^ string_of_expr e))))
              fd.formals actuals;
            prim_of_dt fd.ftype
    | Noexpr -> Void
  in expr
