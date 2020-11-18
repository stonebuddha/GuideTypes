open Core

let version = "0.1.0"
let build_info = "CMU"

let report_result result =
  Or_error.iter_error result ~f:(fun err ->
      let exn = Error.to_exn err in
      Format.eprintf "@.";
      try
        Format.eprintf "%a" Location.report_exception exn; exit 1
      with _ ->
        Format.eprintf "%a@." Error.pp err; exit 1
    )

let parse_file filename =
  Timer.wrap_duration "parsing" (fun () ->
      match Sys.file_exists filename with
      | `No | `Unknown ->
        Error (Error.of_string "file not found")
      | `Yes ->
        let parse_channel ch =
          let lexbuf = Lexing.from_channel ch in
          Location.init lexbuf filename;
          Location.input_name := filename;
          Location.input_lexbuf := Some lexbuf;
          Parse.implementation lexbuf
        in
        In_channel.with_file filename ~f:parse_channel
    )

let typecheck prog =
  Timer.wrap_duration "typechecking" (fun () ->
      Typecheck.tycheck_prog prog
    )

let anf prog =
  Timer.wrap_duration "normalizing" (fun () ->
      Anf.normalize_prog prog
    )

let compile_for_model prog =
  Timer.wrap_duration "emission" (fun () ->
      Compile.emit_prog_for_model Format.std_formatter prog
    )

let compile_for_importance_proposal model proposal =
  Timer.wrap_duration "emission" (fun () ->
      Compile.emit_prog_for_importance_proposal Format.std_formatter model proposal
    )

let cmd_only_parse =
  Command.basic ~summary:"only parse" (
    let open Command.Let_syntax in
    let%map_open filename = anon ("filename" %: Filename.arg_type)
    in
    fun () ->
      let result =
        let open Or_error.Let_syntax in
        let%bind prog = parse_file filename in
        Ok prog
      in
      report_result result
  )

let cmd_type_check =
  Command.basic ~summary:"type check" (
    let open Command.Let_syntax in
    let%map_open filename = anon ("filename" %: Filename.arg_type)
    in
    fun () ->
      let result =
        let open Or_error.Let_syntax in
        let%bind prog = parse_file filename in
        let%bind () = typecheck prog in
        Ok ()
      in
      report_result result
  )

let cmd_normalize =
  Command.basic ~summary:"normalize" (
    let open Command.Let_syntax in
    let%map_open filename = anon ("filename" %: Filename.arg_type)
    in
    fun () ->
      let result =
        let open Or_error.Let_syntax in
        let%bind prog = parse_file filename in
        let%bind () = typecheck prog in
        let iprog = anf prog in
        Ok iprog
      in
      report_result result
  )

let cmd_compile_model =
  Command.basic ~summary:"compile (model)" (
    let open Command.Let_syntax in
    let%map_open filename = anon ("filename" %: Filename.arg_type)
    in
    fun () ->
      let result =
        let open Or_error.Let_syntax in
        let%bind prog = parse_file filename in
        let%bind () = typecheck prog in
        let iprog = anf prog in
        let () = compile_for_model iprog in
        Ok iprog
      in
      report_result result
  )

let cmd_compile_importance_proposal =
  Command.basic ~summary:"compile (importance proposal)" (
    let open Command.Let_syntax in
    let%map_open model_name = flag "-model" (required Filename.arg_type) ~doc:" model"
    and proposal_name = flag "-proposal" (required Filename.arg_type) ~doc:" proposal"
    in
    fun () ->
      let result =
        let open Or_error.Let_syntax in
        let%bind model_prog = parse_file model_name in
        let%bind proposal_prog = parse_file proposal_name in
        let%bind () = typecheck model_prog in
        let%bind () = typecheck proposal_prog in
        let model_iprog = anf model_prog in
        let proposal_iprog = anf proposal_prog in
        let () = compile_for_importance_proposal model_iprog proposal_iprog in
        Ok (model_iprog, proposal_iprog)
      in
      report_result result
  )

let cmd_route =
  Command.group ~summary:"CommInfer" [
    ("only-parse", cmd_only_parse);
    ("type-check", cmd_type_check);
    ("normalize", cmd_normalize);
    ("compile-m", cmd_compile_model);
    ("compile-is", cmd_compile_importance_proposal);
  ]

let () =
  let t1 = Time_now.nanoseconds_since_unix_epoch () |> Time_ns.of_int63_ns_since_epoch in
  at_exit (fun () ->
      let t2 = Time_now.nanoseconds_since_unix_epoch () |> Time_ns.of_int63_ns_since_epoch in
      Format.printf "@.total time: %a@." Time_ns.Span.pp Time_ns.(diff t2 t1)
    );
  Command.run ~version ~build_info cmd_route
