module A = Ast
module S = Sast
module U = Utils

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

let convert_stmt_list_to_sstmt_list (sl:A.stmt list) =
  let rec iter = function
    hd :: tl ->
      

let convert_ast_to_sast (functions:A.func_decl list) (stmts:A.stmt list) =
  S.{
    functions   = [];
    stmts       = [];
  }

let analyze (ast : A.program) = match ast with
  (functions, stmts) ->
    let sast = convert_ast_to_sast functions stmts in
    sast
