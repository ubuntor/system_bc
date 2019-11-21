open Core

let main ~input_file =
  Printf.printf !"Parsed: %{sexp:Ir.Expr.t option}\n" (Parse.lex_and_parse input_file)

let command =
  Command.basic ~summary:"System BC"
    ~readme:(fun () -> "System BC Implementation.")
    Command.Let_syntax.(
      let%map_open input_file = anon ("filename" %: Filename.arg_type) in
      fun () -> main ~input_file)

let () = Command.run command
