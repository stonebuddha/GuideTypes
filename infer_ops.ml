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
  | Hp_nested m -> invalid_hp m.loc

let hp_to_float = function
  | Hp_float f -> Ok (f.txt)
  | Hp_int n -> invalid_hp n.loc
  | Hp_string s -> invalid_hp s.loc
  | Hp_nested m -> invalid_hp m.loc

let hp_to_string = function
  | Hp_string s -> Ok (s.txt)
  | Hp_int n -> invalid_hp n.loc
  | Hp_float f -> invalid_hp f.loc
  | Hp_nested m -> invalid_hp m.loc

let hp_to_nested = function
  | Hp_nested m -> Ok (m.txt)
  | Hp_int n -> invalid_hp n.loc
  | Hp_float f -> invalid_hp f.loc
  | Hp_string s -> invalid_hp s.loc

let construct_algo algo_name hyper_params =
  let hyper_dict = String.Map.of_alist_reduce ~f:(fun _ latest -> latest) hyper_params in
  match algo_name.txt with
  | "importance" ->
    let%bind hp_nsamples =
      match Map.find hyper_dict "nsamples" with
      | Some hp_nsamples -> Ok hp_nsamples
      | None -> Or_error.of_exn (Infer_error ("missing spec for number of samples", algo_name.loc))
    in
    let%bind imp_nsamples = hp_to_int hp_nsamples in
    Ok (Algo_importance { imp_nsamples })

  | "svi" ->
    let%bind svi_niters =
      match Map.find hyper_dict "niters" with
      | Some hp_niters -> hp_to_int hp_niters
      | None -> Or_error.of_exn (Infer_error ("missing spec for number of iterations", algo_name.loc))
    in
    let%bind hp_optim =
      match Map.find hyper_dict "optim" with
      | Some hp_optim -> hp_to_nested hp_optim
      | None -> Or_error.of_exn (Infer_error ("missing spec for optimizer", algo_name.loc))
    in
    let%bind optim_name =
      match Map.find hp_optim "algo" with
      | Some hp_optim_name -> hp_to_string hp_optim_name
      | None -> Or_error.of_exn (Infer_error ("missing spec for inference algorithm", algo_name.loc))
    in
    begin
      match optim_name with
      | "sgd" ->
        begin
          let%bind sgd_lr =
            match Map.find hp_optim "lr" with
            | Some sgd_lr -> hp_to_float sgd_lr
            | None -> Or_error.of_exn (Infer_error ("missing spec for optimizer parameter", algo_name.loc))
          in
          let%bind sgd_momentum =
            match Map.find hp_optim "momentum" with
            | Some sgd_momentum -> hp_to_float sgd_momentum
            | None -> Ok 0.
          in
          Ok (Algo_svi { svi_niters; svi_optim = `SGD { sgd_lr; sgd_momentum } })
        end

      | "adam" ->
        begin
          let%bind adam_lr =
            match Map.find hp_optim "lr" with
            | Some adam_lr -> hp_to_float adam_lr
            | None -> Ok 0.001
          in
          let%bind beta1 =
            match Map.find hp_optim "beta1" with
            | Some beta1 -> hp_to_float beta1
            | None -> Ok 0.9
          in
          let%bind beta2 =
            match Map.find hp_optim "beta2" with
            | Some beta2 -> hp_to_float beta2
            | None -> Ok 0.999
          in
          Ok (Algo_svi { svi_niters; svi_optim = `Adam { adam_lr; adam_betas = (beta1, beta2) } })
        end

      | _ -> Or_error.of_exn (Infer_error ("unknown optimizer", algo_name.loc))
    end

  | _ -> Or_error.of_exn (Infer_error ("unsupported algorithm", algo_name.loc))

let () =
  Location.register_error_of_exn
    (function
      | Infer_error (msg, loc) -> Some (Location.errorf ~loc "%s" msg)
      | _ -> None
    )
