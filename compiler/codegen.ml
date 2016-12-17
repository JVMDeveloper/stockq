module L = Llvm
module A = Ast
module S = Sast
module U = Utils

module StringMap = Map.Make(String)

let translate sast =

  let functions = sast.S.functions
  and stmts = sast.S.stmts
  in

  let context = L.global_context () in
  let the_module = L.create_module context "StockX"
  and i32_t     = L.i32_type    context
  and i8_t      = L.i8_type     context
  and i1_t      = L.i1_type     context
  and void_t    = L.void_type   context 
  and double_t  = L.double_type context in

  (* defines what the func_name is of the "function-less" stmts *)
  let main_func_name = "main" in

  let ltype_of_typ = function
    | A.Int     -> i32_t
    | A.Float   -> double_t
    | A.Bool    -> i1_t
    | A.Void    -> void_t
    | _ -> i32_t
  in

  let ast_type_of_datatype = function
    | A.Primitive(p)    -> p
    | A.Arraytype(p, i) -> p
  in

  let type_of_datatype = function
    | A.Primitive(p)    -> ltype_of_typ p
    | A.Arraytype(p, i) -> ltype_of_typ p
  in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.S.sfname and formal_types =
        Array.of_list (List.map (fun (t, _) -> type_of_datatype t(* ltype_of_typ t *)) fdecl.S.sformals)
      in
      let ftype = L.function_type (type_of_datatype fdecl.S.sftype (* ltype_of_typ fdecl.A.ftype *)) formal_types
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
    let the_function =
      if fdecl.S.sfname = main_func_name then
        let fty = L.function_type i32_t [| |] in
        L.define_function "main" fty the_module
      else
        let get_1_2 (a, _) = a in
        get_1_2 (StringMap.find fdecl.S.sfname function_decls)
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
        if fdecl.S.sfname = main_func_name then
          StringMap.empty
        else
          let add_formal m (t, n) p = L.set_value_name n p;
            let local = L.build_alloca (type_of_datatype t (*ltype_of_typ t*)) n builder in
            ignore (L.build_store p local builder);
            StringMap.add n local m
          in
          List.fold_left2 add_formal StringMap.empty fdecl.S.sformals
            (Array.to_list (L.params the_function))
      in

      let add_local m (t, n) =
        let local_var = L.build_alloca (type_of_datatype t (*ltype_of_typ t*)) n builder
        in StringMap.add n local_var m
      in

      let rec get_locals mylocals = function
        | [] -> mylocals
        | [S.SLocal (t, s, e)] -> get_locals [(t, s)] []
        | S.SLocal (t, s, e) :: r -> get_locals ((t, s) :: mylocals) r
        | _ :: r -> get_locals mylocals r
      in
      List.fold_left add_local formals
        (get_locals [] (if fdecl.S.sfname = main_func_name then stmts else fdecl.S.sbody))
    in

    (* return the value for a variable *)
    let lookup n = StringMap.find n local_vars in

    (* Construct code for expressions in a function *)
    let rec exprgen builder = function
      | S.SIntLiteral i -> L.const_int i32_t i
      | S.SFloatLiteral f -> L.const_float double_t f
      | S.SStringLiteral str -> L.build_global_stringptr str "" builder
      | S.SBoolLiteral b -> L.const_int i1_t (if b then 1 else 0)
      | S.SId s -> L.build_load (lookup s) s builder
      | S.SBinop (e1, op, e2) ->
        let e1' = exprgen builder e1
        and e2' = exprgen builder e2 in
        (match op with
          | A.Add     -> L.build_add    e1' e2' "tmp" builder
          | A.Sub     -> L.build_sub    e1' e2' "tmp" builder
          | A.Mult    -> L.build_mul    e1' e2' "tmp" builder
          | A.Div     -> L.build_sdiv   e1' e2' "tmp" builder
          | A.Mod     -> L.build_srem   e1' e2' "tmp" builder
          | A.And     -> L.build_and    e1' e2' "tmp" builder
          | A.Or      -> L.build_or     e1' e2' "tmp" builder
          | A.Equal   -> L.build_icmp L.Icmp.Eq     e1' e2' "tmp" builder
          | A.Neq     -> L.build_icmp L.Icmp.Ne     e1' e2' "tmp" builder
          | A.Less    -> L.build_icmp L.Icmp.Slt    e1' e2' "tmp" builder
          | A.Leq     -> L.build_icmp L.Icmp.Sle    e1' e2' "tmp" builder
          | A.Greater -> L.build_icmp L.Icmp.Sgt    e1' e2' "tmp" builder
          | A.Geq     -> L.build_icmp L.Icmp.Sge    e1' e2' "tmp" builder

          | A.Addeq   -> exprgen builder (S.SAssign((U.string_of_id e1), S.SBinop(e1, A.Add, e2)))
          | A.Subeq   -> exprgen builder (S.SAssign((U.string_of_id e1), S.SBinop(e1, A.Sub, e2)))
          | A.Multeq  -> exprgen builder (S.SAssign((U.string_of_id e1), S.SBinop(e1, A.Mult, e2)))
          | A.Diveq   -> exprgen builder (S.SAssign((U.string_of_id e1), S.SBinop(e1, A.Div, e2)))
          | A.Modeq   -> exprgen builder (S.SAssign((U.string_of_id e1), S.SBinop(e1, A.Mod, e2)))
        )
      | S.SUnop (uop, e) -> let e' = exprgen builder e in
          (match uop with
            | A.Neg   -> L.build_neg
            | A.Not   -> L.build_not
          ) e' "tmp" builder
      | S.SAssign (s, e) -> let e' = exprgen builder e in
                           ignore (L.build_store e' (lookup s) builder); e'
      | S.SCall ("print", [e]) -> let func e =
                                   let f = (exprgen builder e) in
                                 match e with
        | S.SIntLiteral x -> L.build_call printf_func
                            [| int_format_str; (exprgen builder e) |]
                            "printf" builder
        | S.SStringLiteral x -> L.build_call printf_func
                               [| str_format_str; (exprgen builder e) |]
                               "printf"
                               builder
        | S.SFloatLiteral x -> L.build_call printf_func
                              [| float_format_str; (exprgen builder e) |]
                              "printf" builder
        | S.SBoolLiteral x -> let boolfunc b = match string_of_bool b with
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
      | S.SCall (f, act) ->
          let (fdef, fdecl) = StringMap.find f function_decls in
          let actuals = List.rev (List.map (exprgen builder) (List.rev act)) in
          let result = (match (ast_type_of_datatype fdecl.S.sftype) with A.Void -> ""
                                             | _ -> f ^ "_result")
          in L.build_call fdef (Array.of_list actuals) result builder
      | S.SNoexpr -> L.const_int i32_t 0
    in
    
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
        | Some _ -> ()
        | None -> ignore (f builder)
    in

    (* build the statements in a function *)
    let rec stmtgen builder = function
      | S.SBlock sl -> List.fold_left stmtgen builder sl
      | S.SExpr e -> ignore(exprgen builder e); builder
      | S.SReturn e -> ignore (match (ast_type_of_datatype fdecl.S.sftype) with
        | A.Void -> L.build_ret_void builder
        | _ -> L.build_ret (exprgen builder e) builder); builder

      | S.SIf (e, s1, s2) ->
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

      | S.SFor (e1, e2, e3, s) -> stmtgen builder
          ( S.SBlock [S.SExpr e1 ; S.SWhile (e2, S.SBlock [s ; S.SExpr e3]) ] )
      | S.SWhile (e, s) ->
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
      
      | S.SLocal (t, s, e) -> (match e with
        | S.SNoexpr -> builder
        | _ -> ignore(exprgen builder (S.SAssign(s, e))); builder)
    in

    if fdecl.S.sfname = main_func_name then
      let builder = List.fold_left stmtgen builder stmts in
      ignore(L.build_ret (L.const_int i32_t 0) builder);
    else
      let builder = List.fold_left stmtgen builder fdecl.S.sbody in
      add_terminal builder (match (ast_type_of_datatype fdecl.S.sftype) with
        | A.Void -> L.build_ret_void
        | t -> L.build_ret (L.const_int (ltype_of_typ t) 0));
  in

  List.iter build_function_body functions;
  let mainfdecl = S.{ sftype = A.Primitive(A.Int); sfname = main_func_name; sformals = []; sbody = [] } in
  build_function_body mainfdecl;
  the_module
