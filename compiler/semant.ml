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
    let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
      StringMap.empty (func.formals @ locals)
    in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return the type of an expression or throw an exception *)
    let rec expr = function
      | IntLiteral _ -> Int
      | _ -> Int
    in

    let check_bool_expr e = if expr e != Bool
      then raise (Failure ("expected Boolean expression in " ^ U.string_of_expr e))
      else ()
    in

    (* verify a statement or throw an exception *)
    let rec stmt = function
      | Expr e -> ignore (expr e)
      | _ -> ()
    in

    stmt (Block func.body)

  in
  List.iter check_function functions
