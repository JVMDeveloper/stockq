open Ast


let rec string_of_func func =
  "def " ^ string_of_datatype func.ftype ^ " " ^ func.fname ^ " (" ^
  String.concat ", " (List.map snd func.formals) ^ ") {\n\t" ^
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
