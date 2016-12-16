type action = Tokenize | Parserize | Ast | LLVM_IR | Compile

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-t", Tokenize);
                              ("-p", Parserize);
                              ("-c", Compile);
                              ("-a",Ast);
                              ("-l",LLVM_IR)] (* Generate LLVM, don't check *)
  else Compile in
  let lexbuf = Lexing.from_channel stdin in
  match action with
    | Tokenize -> 
        let rec print_tokens = function
          | Parser.EOF -> print_endline ""
          | token ->
              print_endline (Utils.string_of_token token);
              print_tokens (Scanner.token lexbuf)
        in
        print_tokens (Scanner.token lexbuf)
    | Parserize ->
        let ast = Parser.program Scanner.token lexbuf in
        let result = Utils.string_of_program [] ast in
        print_endline result
    | _ ->
        let ast = Parser.program Scanner.token lexbuf in
        Semant.check ast;
        match action with
          | Ast -> print_string (Utils.string_of_program [] ast)
          | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate ast))
          | Compile -> let m = Codegen.translate ast in
            Llvm_analysis.assert_valid_module m;
            print_string (Llvm.string_of_llmodule m)
          | _ -> ()
