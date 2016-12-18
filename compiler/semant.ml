(* Semantic checking for the StockQ compiler *)

open Ast

module U = Utils

module StringMap = Map.Make(String)

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.
*)

let check (functions, stmts) =

  let main_func_name = "main" in

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
      | n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
    | (Primitive(Void), n) -> raise (Failure (exceptf n))
    | _ -> ()
  in

  (* Raise an exception if the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
    if lvaluet == rvaluet then lvaluet else raise err
  in

  let prim_of_dt = function
    | Primitive(p)      -> p
    | Arraytype(p, i)   -> p
  in

  (**** Checking Functions ****)

  if List.mem "print" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else ();

  if List.mem main_func_name (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function main may not be defined")) else ();

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);

  (* Function declaration for a named function *)
  let print_type = Primitive(Int) in
  let built_in_decls = StringMap.singleton "print"
    { ftype = Primitive(Void);
      fname = "print";
      formals = [(print_type, "x")];
      body = [] }
  in

  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                        built_in_decls functions
  in

  let function_decl s = try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let check_function func =
    (* check formals *)
    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);

    (* check locals *)
    let locals =
      let rec get_locals mylocals = function
        | [] -> mylocals
        | [Local (t, s, e)] -> get_locals [(t, s)] []
        | Local (t, s, e) :: r -> get_locals ((t, s) :: mylocals) r
        | _ :: r -> get_locals mylocals r
      in
      get_locals [] (if func.fname = main_func_name then stmts else func.body)
    in

    List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
      " in " ^ func.fname)) locals;

    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
      (List.map snd locals);

    (* Datatype of each variable (formal, or local *)
    let symbols =
      List.fold_left (fun m (t, n) -> StringMap.add n (prim_of_dt t) m)
        StringMap.empty (func.formals @ locals)
    in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return the type of an expression or throw an exception *)
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
            (* ops on Int/Float *)
            | Add | Sub | Mult | Div when t1 = Int && t2 = Int -> Int
            | Add | Sub | Mult | Div when t1 = Int && t2 = Float -> Float
            | Add | Sub | Mult | Div when t1 = Float && t2 = Int -> Float
            | Add | Sub | Mult | Div when t1 = Float && t2 = Float -> Float
            
            (* ops on Bools *)
            | Equal | Neq when t1 = t2 -> Bool
            | Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
            | And | Or when t1 = Bool && t2 = Bool -> Bool
            | _ -> raise (Failure ("illegal binary operator " ^
                U.string_of_primitive t1 ^ " " ^ U.string_of_op op ^ " " ^
                U.string_of_primitive t2 ^ " in " ^ U.string_of_expr e))
          )
      | Unop(op, e) as ex -> let t = expr e in
          (match op with
            | Neg when t = Int -> Int
            | Neg when t = Float -> Float
            | Not when t = Bool -> Bool
            | _ -> raise (Failure ("illegal unary op " ^ U.string_of_uop op ^
                    U.string_of_primitive t ^ " in " ^ U.string_of_expr ex))
          )
      | Assign(var, e) as ex ->
          let lt = type_of_identifier var
          and rt = expr e in
          check_assign lt rt (Failure ("illegal assignment " ^
            U.string_of_primitive lt ^ " = " ^ U.string_of_primitive rt ^
            " in " ^ U.string_of_expr ex))
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
                U.string_of_expr call))
            else
              List.iter2 (fun (ft, _) e ->
                let ft = prim_of_dt ft in
                let et = expr e in
                ignore (check_assign ft et
                  (Failure ("illegal actual argument found " ^
                    U.string_of_primitive et ^ " expected " ^
                    U.string_of_primitive et ^ " in " ^ U.string_of_expr e))))
                fd.formals actuals;
              prim_of_dt fd.ftype
      | Noexpr -> Void
    in

    let check_bool_expr e = if expr e != Bool
      then raise (Failure ("expected Boolean expression in " ^ U.string_of_expr e))
      else ()
    in

    (* verify a statement or throw an exception *)
    let rec stmt = function
      | Block sl ->
          let rec check_block = function
            | [] -> ()
            | [Return _ as s] -> stmt s
            | Return _ :: _ -> raise (Failure ("nothing may follow a return"))
            | Block sl :: ss -> check_block (sl @ ss)
            | s :: ss -> stmt s; check_block ss
          in
          check_block sl
      | Expr e -> ignore (expr e)
      | Return e ->
          let t = expr e in
          if t = (prim_of_dt func.ftype) then ()
          else
            raise (Failure ("return gives " ^ U.string_of_primitive t ^
              " expected " ^ U.string_of_primitive (prim_of_dt func.ftype) ^
              " in " ^ U.string_of_expr e))
      | If(p, b1, b2) -> check_bool_expr p; stmt b1; stmt b2
      | For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
                               ignore (expr e3); stmt st
      | While(p, s) -> check_bool_expr p; stmt s
      | Local(dt, var, e) ->
          ignore (expr e);
          (match (expr e) with
            | Void -> ()
            | _ ->
                let lt = type_of_identifier var
                and rt = expr e in
                ignore (check_assign lt rt (Failure ("illegal assignment " ^
                  U.string_of_primitive lt ^ " = " ^ U.string_of_primitive rt ^
                  " in " ^ U.string_of_expr e)))
          )
    in

    stmt (if func.fname = main_func_name then Block(stmts) else Block(func.body))

  in
  List.iter check_function functions;
  let mainfdecl = { ftype = Primitive(Int); fname = main_func_name;
                    formals = []; body = [] } in
  check_function mainfdecl
