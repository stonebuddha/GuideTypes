open Core

let report_error result =
  Result.iter_error result ~f:(fun exn -> Location.report_exception Format.err_formatter exn; exit 1)

let parse_file filename =
  let parse_channel ch =
    let lexbuf = Lexing.from_channel ch in
    Location.init lexbuf filename;
    Location.input_name := filename;
    Location.input_lexbuf := Some lexbuf;
    Parse.implementation lexbuf
  in
  In_channel.with_file filename ~f:parse_channel

let cmd_only_parse =
  let open Command.Let_syntax in
  Command.basic ~summary:"only parse" (
    let%map_open filename = anon ("filename" %: Filename.arg_type)
    in
    fun () ->
      let result =
        let open Result.Let_syntax in
        let%bind prog = parse_file filename in
        Ok prog
      in
      report_error result
  )

let cmd_route =
  Command.group ~summary:"CommInfer" [
    ("only-parse", cmd_only_parse);
  ]

let () =
  Command.run ~version:"0.1.0" ~build_info:"CMU" cmd_route
