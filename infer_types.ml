open Core
open Ast_types

type hyper_param =
  | Hp_int of int loc
  | Hp_float of float loc
  | Hp_string of string loc
  | Hp_nested of hyper_param String.Map.t loc

type importance = {
  imp_nsamples: int;
}

type svi = {
  svi_niters: int;
  svi_optim: [ `SGD of sgd_option | `Adam of adam_option ];
}

and sgd_option = {
  sgd_lr: float;
  sgd_momentum: float;
}

and adam_option = {
  adam_lr: float;
  adam_betas: float * float;
}

type algo =
  | Algo_importance of importance
  | Algo_svi of svi

type script = {
  inf_algo: algo;
  inf_model: procedure_id * exp option list * exp list;
  inf_guide: procedure_id * exp option list * exp list;
  inf_input: channel_id * string loc;
  inf_output: channel_id * string loc;
}
