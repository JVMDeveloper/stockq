type action = Tokenize | Parserize | Compile

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-t", Tokenize);
                              ("-p", Parserize);
                              ("-c", Compile) ]
  else Compile in
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.token lexbuf in
  Semant.check ast;
  match action with
  | Tokenize -> 
      let rec print_tokens = function
        | Parser.EOF -> ""
        | token ->
            print_endline (Utils.string_of_token token);
            print_tokens (Scanner.token lexbuf)
      in
      print_tokens (Scanner.token lexbuf)
  | Parserize ->
      let result = Utils.string_of_program [] ast in
      (print_endline result); ""

