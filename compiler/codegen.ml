module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (func_decls, stmts) =
  let context = L.global_context () in
  let the_module = L.create_module context "StockX"
  and i32_t    = L.i32_type   context
  and i8_t     = L.i8_type    context
  and i1_t     = L.i1_type    context
  and double_t = L.double_type context in

  let ltype_of_typ = function
    | A.Int -> i32_t
    | A.Float -> double_t
    | A.Bool -> i1_t
    | _ -> i32_t
  in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* build the entry point for the llvm main function *) 
  let fty = L.function_type i32_t [| |] in
  let f = L.define_function "main" fty the_module in
  let builder = L.builder_at_end context (L.entry_block f) in

  (* print formatters *)
  let str_format_str   = L.build_global_stringptr "%s\n" "fmt" builder in
  let int_format_str   = L.build_global_stringptr "%d\n" "fmt" builder in
  let float_format_str = L.build_global_stringptr "%f\n" "fmt" builder in

  let add_terminal builder f =
    match L.block_terminator (L.insertion_block builder) with
      | Some _ -> ()
      | None -> ignore (f builder)
  in

  let main_vars =
    let add_local m (t, n) =
      let local_var = L.build_alloca (ltype_of_typ t) n builder
      in StringMap.add n local_var m
    in

    let rec get_locals mylocals = function
      | [] -> mylocals
      | [A.Local (t, s, e)] -> get_locals [(t, s)] []
      | A.Local (t, s, e) :: r -> get_locals ((t, s) :: mylocals) r
      | _ :: r -> get_locals mylocals r
    in
    List.fold_left add_local StringMap.empty (get_locals [] stmts)
  in

  let func_vars func_scope fbuilder =
    let add_local m (t, n) =
      let local_var = L.build_alloca (ltype_of_typ t) n fbuilder
      in StringMap.add n local_var m
    in

    let rec get_locals mylocals = function
      | [] -> mylocals
      | [A.Local (t, s, e)] -> get_locals [(t, s)] []
      | A.Local (t, s, e) :: r -> get_locals ((t, s) :: mylocals) r
      | _ :: r -> get_locals mylocals r
    in

    List.fold_left add_local StringMap.empty (get_locals [] func_scope)
  in

  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname and formal_types =
        Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.A.formals)
      in
      let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types
      in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m
    in
    List.fold_left function_decl StringMap.empty func_decls
  in

  (* lookups for main and for a function *)
  let lookup n = StringMap.find n main_vars in
  let func_lookup n func_scope fbuilder = StringMap.find n (func_vars func_scope fbuilder) in

  (* Construct code for an expression; return its value *)
  let rec exprgen builder = function
    | A.IntLiteral i -> L.const_int i32_t i
    | A.FloatLiteral f -> L.const_float double_t f
    | A.StringLiteral str -> L.build_global_stringptr str "" builder
    | A.BoolLiteral b -> L.const_int i1_t (if b then 1 else 0)
    | A.Id s -> L.build_load (lookup s) s builder

    (* e1 and e2 are always same type -> do in semantic checker *)
    | A.Binop (e1, op, e2) ->
      let e1' = exprgen builder e1
      and e2' = exprgen builder e2 in
      (match op with
        | A.Add     -> L.build_add
        | A.Sub     -> L.build_sub
        | A.Mult    -> L.build_mul
        | A.Div     -> L.build_sdiv
        | A.Mod     -> L.build_add (* NEEDS CHANGING *)
        | A.And     -> L.build_and
        | A.Or      -> L.build_or
        | A.Equal   -> L.build_icmp L.Icmp.Eq
        | A.Neq     -> L.build_icmp L.Icmp.Ne
        | A.Less    -> L.build_icmp L.Icmp.Slt
        | A.Leq     -> L.build_icmp L.Icmp.Sle
        | A.Greater -> L.build_icmp L.Icmp.Sgt
        | A.Geq     -> L.build_icmp L.Icmp.Sge
      ) e1' e2' "tmp" builder
        
    | A.Unop (uop, e) -> let e' = exprgen builder e in
        (match uop with
          | A.Neg   -> L.build_neg
          | A.Not   -> L.build_not
        ) e' "tmp" builder
    | A.Assign (s, e) -> let e' = exprgen builder e in
                         ignore (L.build_store e' (lookup s) builder); e'
    | A.Call ("print", [e]) -> let func e =
                                 let f = (exprgen builder e) in
                               match e with
      | A.IntLiteral x -> L.build_call printf_func
                          [| int_format_str; (exprgen builder e) |]
                          "printf" builder
      | A.StringLiteral x -> L.build_call printf_func
                             [| str_format_str; (exprgen builder e) |]
                             "printf"
                             builder
      | A.FloatLiteral x -> L.build_call printf_func
                            [| float_format_str; (exprgen builder e) |]
                            "printf" builder
      | A.BoolLiteral x -> let boolfunc b = match string_of_bool b with
            | "true" -> L.build_call printf_func
                        [| str_format_str;
                           (L.build_global_stringptr "'true'" "" builder)
                        |] "printf" builder
            | "false" -> L.build_call printf_func
                         [| str_format_str;
                            (L.build_global_stringptr "'false'" "" builder)
                         |] "printf" builder
            | _ -> L.build_global_stringptr "---error---" "" builder
            in boolfunc x
      | _ -> L.build_call printf_func
                          [| int_format_str; f |]
                          "printf" builder
      in func e
    (* or another A.Call *)
    | A.Call (f, act) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let actuals = List.rev (List.map (exprgen builder) (List.rev act)) in
        let result = (match fdecl.A.typ with A.Void -> ""
                                           | _ -> f ^ "_result")
        in L.build_call fdef (Array.of_list actuals) result builder
    | A.Noexpr -> L.const_int i32_t 0
  in

  (* Build the code for the given statement; return the builder for
     the statement's successor *)
  let rec stmtgen builder = function
    | A.Block sl -> List.fold_left (stmtgen) builder sl
    | A.Expr e -> ignore (exprgen builder e); builder
    | A.Return e -> ignore (exprgen builder e); builder
    | A.If (e, s1, s2) ->
        let bool_val = exprgen builder e in
        let merge_bb = L.append_block context "merge" f in

        let then_bb = L.append_block context "then" f in
        add_terminal (stmtgen (L.builder_at_end context then_bb) s1)
        (L.build_br merge_bb);

        let else_bb = L.append_block context "else" f in
        add_terminal (stmtgen (L.builder_at_end context else_bb) s2)
        (L.build_br merge_bb);

        ignore (L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context merge_bb

    | A.For (e1, e2, e3, s) -> builder
    | A.While (e, s) ->
        let pred_bb = L.append_block context "while" f
        in
        ignore (L.build_br pred_bb builder);

        let body_bb = L.append_block context "while_body" f
        in
        add_terminal (stmtgen (L.builder_at_end context body_bb) s)
        (L.build_br pred_bb);

        let pred_builder = L.builder_at_end context pred_bb
        in
        let bool_val = exprgen pred_builder e
        in

        let merge_bb = L.append_block context "merge" f
        in
        ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
        L.builder_at_end context merge_bb

    | A.Local (t, s, Noexpr) -> builder
    | A.Local (t, s, e) -> ignore (exprgen builder (A.Assign(s, e))); builder
  in

  (* Construct code for expressions in a function *)
  let rec func_exprgen func_scope builder = function
    | A.IntLiteral i -> L.const_int i32_t i
    | A.FloatLiteral f -> L.const_float double_t f
    | A.StringLiteral str -> L.build_global_stringptr str "" builder
    | A.BoolLiteral b -> L.const_int i1_t (if b then 1 else 0)
    | A.Id s -> L.build_load (func_lookup s func_scope builder) s builder

    (* e1 and e2 are always same type -> do in semantic checker *)
    | A.Binop (e1, op, e2) ->
      let e1' = func_exprgen func_scope builder e1
      and e2' = func_exprgen func_scope builder e2 in
      (match op with
        | A.Add     -> L.build_add
        | A.Sub     -> L.build_sub
        | A.Mult    -> L.build_mul
        | A.Div     -> L.build_sdiv
        | A.Mod     -> L.build_add (* NEEDS CHANGING *)
        | A.And     -> L.build_and
        | A.Or      -> L.build_or
        | A.Equal   -> L.build_icmp L.Icmp.Eq
        | A.Neq     -> L.build_icmp L.Icmp.Ne
        | A.Less    -> L.build_icmp L.Icmp.Slt
        | A.Leq     -> L.build_icmp L.Icmp.Sle
        | A.Greater -> L.build_icmp L.Icmp.Sgt
        | A.Geq     -> L.build_icmp L.Icmp.Sge
      ) e1' e2' "tmp" builder
        
    | A.Unop (uop, e) -> let e' = func_exprgen func_scope builder e in
        (match uop with
          | A.Neg   -> L.build_neg
          | A.Not   -> L.build_not
        ) e' "tmp" builder
    | A.Assign (s, e) -> let e' = func_exprgen func_scope builder e in
                         ignore (L.build_store e' (func_lookup s func_scope builder) builder); e'
    | A.Call ("print", [e]) -> let func e =
                                 let f = (func_exprgen func_scope builder e) in
                               match e with
      | A.IntLiteral x -> L.build_call printf_func
                          [| int_format_str; (func_exprgen func_scope builder e) |]
                          "printf" builder
      | A.StringLiteral x -> L.build_call printf_func
                             [| str_format_str; (func_exprgen func_scope builder e) |]
                             "printf"
                             builder
      | A.FloatLiteral x -> L.build_call printf_func
                            [| float_format_str; (func_exprgen func_scope builder e) |]
                            "printf" builder
      | A.BoolLiteral x -> let boolfunc b = match string_of_bool b with
            | "true" -> L.build_call printf_func
                        [| str_format_str;
                           (L.build_global_stringptr "'true'" "" builder)
                        |] "printf" builder
            | "false" -> L.build_call printf_func
                         [| str_format_str;
                            (L.build_global_stringptr "'false'" "" builder)
                         |] "printf" builder
            | _ -> L.build_global_stringptr "---error---" "" builder
            in boolfunc x
      | _ -> L.build_call printf_func
                          [| int_format_str; f |]
                          "printf" builder
      in func e
    (* or another A.Call *)
    | A.Call (f, act) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let actuals = List.rev (List.map (func_exprgen func_scope builder) (List.rev act)) in
        let result = (match fdecl.A.typ with A.Void -> ""
                                           | _ -> f ^ "_result")
        in L.build_call fdef (Array.of_list actuals) result builder
    | A.Noexpr -> L.const_int i32_t 0
  in

  (* build the statements in a function *)
  let rec func_stmtgen func_scope func_stmts builder = function
    | A.Block sl -> List.fold_left (func_stmtgen func_scope func_stmts) builder sl
    | A.Expr e -> ignore (func_exprgen func_stmts builder e); builder
    | A.Return e -> ignore (func_exprgen func_stmts builder e); builder
    | A.If (e, s1, s2) ->
        let bool_val = func_exprgen func_stmts builder e in
        let merge_bb = L.append_block context "merge" func_scope in

        let then_bb = L.append_block context "then" func_scope in
        add_terminal (func_stmtgen func_scope func_stmts (L.builder_at_end context then_bb) s1)
        (L.build_br merge_bb);

        let else_bb = L.append_block context "else" func_scope in
        add_terminal (func_stmtgen func_scope func_stmts (L.builder_at_end context else_bb) s2)
        (L.build_br merge_bb);

        ignore (L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context merge_bb

    | A.For (e1, e2, e3, s) -> builder
    | A.While (e, s) ->
        let pred_bb = L.append_block context "while" func_scope
        in
        ignore (L.build_br pred_bb builder);

        let body_bb = L.append_block context "while_body" func_scope
        in
        add_terminal (func_stmtgen func_scope func_stmts (L.builder_at_end context body_bb) s)
        (L.build_br pred_bb);

        let pred_builder = L.builder_at_end context pred_bb
        in
        let bool_val = func_exprgen func_stmts pred_builder e
        in

        let merge_bb = L.append_block context "merge" func_scope
        in
        ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
        L.builder_at_end context merge_bb

    | A.Local (t, s, Noexpr) -> builder
    | A.Local (t, s, e) -> ignore (func_exprgen func_stmts builder (A.Assign(s, e))); builder
  in

  (* builds statements outside of any functions in stockq *)
  let build_statements builder stmts =
    let builder = List.fold_left stmtgen builder stmts in
    ignore(L.build_ret (L.const_int i32_t 0) builder);
  in

  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* build the code for each statement in the function *)
    let builder = func_stmtgen the_function fdecl.A.body builder (A.Block fdecl.A.body) in

    (* add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
      | A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in
  
  ignore (List.iter build_function_body func_decls);
  build_statements builder stmts;
  the_module
