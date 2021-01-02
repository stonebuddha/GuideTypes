open Core
open Ast_types
open Infer_types
open Or_error.Let_syntax

exception Infer_error of string * Location.t

let invalid_hp loc =
  Or_error.of_exn (Infer_error ("expected integer-valued hyper parameter", loc))

let hp_to_int = function
  | Hp_int n -> Ok (n.txt)
  | Hp_float f -> invalid_hp f.loc
  | Hp_string s -> invalid_hp s.loc

let hp_to_float = function
  | Hp_float f -> Ok (f.txt)
  | Hp_int n -> invalid_hp n.loc
  | Hp_string s -> invalid_hp s.loc

let hp_to_string = function
  | Hp_string s -> Ok (s.txt)
  | Hp_int n -> invalid_hp n.loc
  | Hp_float f -> invalid_hp f.loc

let construct_algo algo_name hyper_params =
  let%bind hyper_dict = String.Map.of_alist_or_error hyper_params in
  match algo_name.txt with
  | "importance" ->
    let%bind hp_nsamples = Map.find_or_error hyper_dict "nsamples" in
    let%bind imp_nsamples = hp_to_int hp_nsamples in
    Ok (Algo_importance { imp_nsamples })
  | _ -> Or_error.of_exn (Infer_error ("unsupported algorithm", algo_name.loc))

let () =
  Location.register_error_of_exn
    (function
      | Infer_error (msg, loc) -> Some (Location.errorf ~loc "%s" msg)
      | _ -> None
    )
