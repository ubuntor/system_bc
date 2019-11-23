open Core

let main ~input_file =
  match Parse.lex_and_parse input_file with
  | Some parsed -> (
      Printf.printf !"Parsed successfully: %{sexp:Ir.Expr.t}\n" parsed;
      match Check.typecheck parsed with
      | Ok ty ->
          Printf.printf !"Type: %{sexp:Ir.Ty.t}\n" ty;
          Printf.printf
            !"Evaluated: %{sexp:Ir.Expr.t}\n"
            (Interpreter.evaluate parsed)
      | Error e -> Printf.printf !"Type error: %{sexp:Error.t}\n" e )
  | None -> Printf.printf !"Failed to parse"

let command =
  Command.basic ~summary:"System BC"
    ~readme:(fun () -> "System BC Implementation.")
    Command.Let_syntax.(
      let%map_open input_file = anon ("filename" %: Filename.arg_type) in
      fun () -> main ~input_file)

let () = Command.run command
