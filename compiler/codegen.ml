open Ast

module L = Llvm
module U = Utils

module StringMap = Map.Make(String)

let translate (functions, stmts) =
  let context = L.global_context () in
  let the_module = L.create_module context "StockX"
  and i32_t     = L.i32_type    context
  and i8_t      = L.i8_type     context in
  let str_t     = L.pointer_type i8_t
  and i1_t      = L.i1_type     context
  and void_t    = L.void_type   context 
  and double_t  = L.double_type context in

  (* defines what the func_name is of the "function-less" stmts *)
  let main_func_name = "main" in

  let ltype_of_typ = function
    | Int     -> i32_t
    | Float   -> double_t
    | Bool    -> i1_t
    | String  -> str_t
    | Void    -> void_t
    | _ -> i32_t
  in

  let ast_type_of_datatype = function
    | Primitive(p)    -> p
    | Arraytype(p, _) -> p
  in

  let type_of_datatype = function
    | Primitive(p)    -> ltype_of_typ p
    | Arraytype(p, _) -> ltype_of_typ p
  in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.fname and formal_types =
        Array.of_list (List.map (fun (t, _) -> type_of_datatype t) fdecl.formals)
      in
      let ftype = L.function_type (type_of_datatype fdecl.ftype) formal_types
      in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m
    in
    List.fold_left function_decl StringMap.empty functions
  in

  (* Fill in the body of the given function
   * 't' indicates whether we are working on
   * a user-defined function, or the "main"
   * function that encompasses all of the
   * statements outside of any functions.
   * t=0 -> functions, t=1 -> statements
   *)
  let build_function_body fdecl =

    let type_of_expr e = U.type_of_expr fdecl functions e in
    let the_function =
      if fdecl.fname = main_func_name then
        let fty = L.function_type i32_t [| |] in
        L.define_function "main" fty the_module
      else
        let get_1_2 (a, _) = a in
        get_1_2 (StringMap.find fdecl.fname function_decls)
    in

    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* print formatters *)
    let str_format_str   = L.build_global_stringptr "%s\n" "fmt" builder in
    let int_format_str   = L.build_global_stringptr "%d\n" "fmt" builder in
    let float_format_str = L.build_global_stringptr "%f\n" "fmt" builder in

    (* construct the function's "locals" *)
    let local_vars =
      (* get formals first, if this is a function decl *)
      let formals =
        if fdecl.fname = main_func_name then
          StringMap.empty
        else
          let add_formal m (t, n) p = L.set_value_name n p;
            let local = L.build_alloca (type_of_datatype t) n builder in
            ignore (L.build_store p local builder);
            StringMap.add n local m
          in
          List.fold_left2 add_formal StringMap.empty fdecl.formals
            (Array.to_list (L.params the_function))
      in

      let add_local m (t, n) =
        let local_var = L.build_alloca (type_of_datatype t) n builder
        in StringMap.add n local_var m
      in

      let rec get_locals mylocals = function
        | [] -> mylocals
        | [Local (t, s, _)] -> get_locals [(t, s)] []
        | Local (t, s, _) :: r -> get_locals ((t, s) :: mylocals) r
        | _ :: r -> get_locals mylocals r
      in
      List.fold_left add_local formals
        (get_locals [] (if fdecl.fname = main_func_name then stmts else fdecl.body))
    in

    (* return the value for a variable *)
    let lookup n = StringMap.find n local_vars in

    (* Construct code for expressions in a function *)
    let rec exprgen builder = function
      | IntLiteral i -> L.const_int i32_t i
      | FloatLiteral f -> L.const_float double_t f
      | StringLiteral str -> L.build_global_stringptr str "" builder
      | BoolLiteral b -> L.const_int i1_t (if b then 1 else 0)
      | Id s -> L.build_load (lookup s) s builder
      | Binop (e1, op, e2) ->
        let e1' = exprgen builder e1
        and e2' = exprgen builder e2
        and t1 = type_of_expr e1
        and t2 = type_of_expr e2 in
        let e1' = if t1 = Int && t2 = Float then
                    (L.const_sitofp e1' double_t)
                  else e1'
        and e2' = if t1 = Float && t2 = Int then
                    (L.const_sitofp e2' double_t)
                  else e2'
        in
        (match op with
          | Add when t1 = Float || t2 = Float -> L.build_fadd e1' e2' "tmp" builder
          | Add when t1 = Int && t2 = Int -> L.build_add e1' e2' "tmp" builder

          | Sub when t1 = Float || t2 = Float -> L.build_fsub e1' e2' "tmp" builder
          | Sub when t1 = Int && t2 = Int -> L.build_sub e1' e2' "tmp" builder

          | Mult when t1 = Float || t2 = Float -> L.build_fmul e1' e2' "tmp" builder
          | Mult when t1 = Int && t2 = Int -> L.build_mul e1' e2' "tmp" builder

          | Div when t1 = Float || t2 = Float -> L.build_fdiv e1' e2' "tmp" builder
          | Div when t1 = Int && t2 = Int -> L.build_sdiv e1' e2' "tmp" builder

          | Mod when t1 = Float || t2 = Float -> L.build_frem e1' e2' "tmp" builder
          | Mod when t1 = Int && t2 = Int -> L.build_srem e1' e2' "tmp" builder

          | And     -> L.build_and    e1' e2' "tmp" builder
          | Or      -> L.build_or     e1' e2' "tmp" builder

          | Equal when t1 = Float || t2 = Float -> L.build_fcmp L.Fcmp.Ueq e1' e2' "tmp" builder
          | Equal   -> L.build_icmp L.Icmp.Eq     e1' e2' "tmp" builder

          | Neq when t1 = Float || t2 = Float -> L.build_fcmp L.Fcmp.Une e1' e2' "tmp" builder
          | Neq     -> L.build_icmp L.Icmp.Ne     e1' e2' "tmp" builder

          | Less when t1 = Float || t2 = Float -> L.build_fcmp L.Fcmp.Ult e1' e2' "tmp" builder
          | Less when t1 = Int && t2 = Int -> L.build_icmp L.Icmp.Slt e1' e2' "tmp" builder

          | Leq when t1 = Float || t2 = Float -> L.build_fcmp L.Fcmp.Ule e1' e2' "tmp" builder
          | Leq when t1 = Int && t2 = Int -> L.build_icmp L.Icmp.Sle e1' e2' "tmp" builder

          | Greater when t1 = Float || t2 = Float -> L.build_fcmp L.Fcmp.Ugt e1' e2' "tmp" builder
          | Greater when t1 = Int && t2 = Int -> L.build_icmp L.Icmp.Sgt e1' e2' "tmp" builder

          | Geq when t1 = Float || t2 = Float -> L.build_fcmp L.Fcmp.Uge e1' e2' "tmp" builder
          | Geq when t1 = Int && t2 = Int -> L.build_icmp L.Icmp.Sge e1' e2' "tmp" builder

          | Addeq   -> exprgen builder (Assign((U.string_of_id e1), Binop(e1, Add, e2)))
          | Subeq   -> exprgen builder (Assign((U.string_of_id e1), Binop(e1, Sub, e2)))
          | Multeq  -> exprgen builder (Assign((U.string_of_id e1), Binop(e1, Mult, e2)))
          | Diveq   -> exprgen builder (Assign((U.string_of_id e1), Binop(e1, Div, e2)))
          | Modeq   -> exprgen builder (Assign((U.string_of_id e1), Binop(e1, Mod, e2)))
          
          | _ -> exprgen builder Noexpr
        )
      | Unop (uop, e) -> let e' = exprgen builder e
                           and t = type_of_expr e in
          (match uop with
            | Neg when t = Float -> L.build_fneg
            | Neg   -> L.build_neg
            | Not   -> L.build_not
          ) e' "tmp" builder
      | Assign (s, e) -> let e' = exprgen builder e in
                           ignore (L.build_store e' (lookup s) builder); e'
      | Call ("print", [e]) -> 
          (match (type_of_expr e) with
            | Int -> L.build_call printf_func [| int_format_str;
                            (exprgen builder e) |] "printf" builder
            | Float -> L.build_call printf_func [| float_format_str;
                            (exprgen builder e) |] "printf" builder
            | String -> L.build_call printf_func [| str_format_str;
                            (exprgen builder e) |] "printf" builder
            | Bool -> L.build_call printf_func [| int_format_str;
                            (exprgen builder e) |] "printf" builder
            | _ -> L.build_call printf_func [| int_format_str;
                        (exprgen builder e) |] "printf" builder
          )

      | Call (f, act) ->
          let (fdef, fdecl) = StringMap.find f function_decls in
          let actuals = List.rev (List.map (exprgen builder) (List.rev act)) in
          let result = (match (ast_type_of_datatype fdecl.ftype) with Void -> ""
                                             | _ -> f ^ "_result")
          in L.build_call fdef (Array.of_list actuals) result builder
      | Noexpr -> L.const_int i32_t 0
    in
    
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
        | Some _ -> ()
        | None -> ignore (f builder)
    in

    (* build the statements in a function *)
    let rec stmtgen builder = function
      | Block sl -> List.fold_left stmtgen builder sl
      | Expr e -> ignore(exprgen builder e); builder
      | Return e -> ignore (match (ast_type_of_datatype fdecl.ftype) with
        | Void -> L.build_ret_void builder
        | _ -> L.build_ret (exprgen builder e) builder); builder

      | If (e, s1, s2) ->
        let bool_val = exprgen builder e in
        let merge_bb = L.append_block context "merge" the_function in

        let then_bb = L.append_block context "then" the_function in
        add_terminal (stmtgen (L.builder_at_end context then_bb) s1)
        (L.build_br merge_bb);

        let else_bb = L.append_block context "else" the_function in
        add_terminal (stmtgen (L.builder_at_end context else_bb) s2)
        (L.build_br merge_bb);

        ignore (L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context merge_bb

      | For (e1, e2, e3, s) -> stmtgen builder
          ( Block [Expr e1 ; While (e2, Block [s ; Expr e3]) ] )
      | While (e, s) ->
        let pred_bb = L.append_block context "while" the_function
        in
        ignore (L.build_br pred_bb builder);

        let body_bb = L.append_block context "while_body" the_function
        in
        add_terminal (stmtgen (L.builder_at_end context body_bb) s)
        (L.build_br pred_bb);

        let pred_builder = L.builder_at_end context pred_bb
        in
        let bool_val = exprgen pred_builder e
        in

        let merge_bb = L.append_block context "merge" the_function
        in
        ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
        L.builder_at_end context merge_bb
      
      | Local (_, s, e) -> (match e with
        | Noexpr -> builder
        | _ -> ignore(exprgen builder (Assign(s, e))); builder)
    in

    if fdecl.fname = main_func_name then
      let builder = List.fold_left stmtgen builder stmts in
      ignore(L.build_ret (L.const_int i32_t 0) builder);
    else
      let builder = List.fold_left stmtgen builder fdecl.body in
      add_terminal builder (match (ast_type_of_datatype fdecl.ftype) with
        | Void -> L.build_ret_void
        | t -> L.build_ret (L.const_int (ltype_of_typ t) 0));
  in

  List.iter build_function_body functions;
  let mainfdecl = { ftype = Primitive(Int); fname = main_func_name; formals = []; body = stmts } in
  build_function_body mainfdecl;
  the_module
