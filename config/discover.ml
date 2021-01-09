module C = Configurator.V1

let () =
  C.main ~name:"torch-config" (fun _ ->
      let empty_flags = { C.Pkg_config.cflags = []; libs = [] } in
      let config include_dir =
        let cflags =
          [ "-isystem"
          ; Printf.sprintf "%s" include_dir
          ; "-isystem"
          ; Printf.sprintf "%s/torch/csrc/api/include" include_dir
          ]
        in
        { C.Pkg_config.cflags; libs = [] }
      in
      let conf =
        match Sys.getenv_opt "LIBTORCH" with
        | Some l -> config (Filename.concat l "include")
        | None ->
          match Sys.getenv_opt "OPAM_SWITCH_PREFIX" with
          | Some prefix ->
            let lib_dir = Filename.(concat (concat prefix "lib") "libtorch") in
            if Sys.file_exists lib_dir
            then config (Filename.concat lib_dir "include")
            else empty_flags
          | None -> empty_flags
      in
      C.Flags.write_sexp "cxx_flags.sexp" conf.cflags
    )
