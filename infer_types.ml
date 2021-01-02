open Ast_types

type hyper_param =
  | Hp_int of int loc
  | Hp_float of float loc
  | Hp_string of string loc

type importance = {
  imp_nsamples: int;
}

type algo =
  | Algo_importance of importance
  | Algo_svi

type script = {
  inf_algo: algo;
  inf_model: procedure_id * exp list;
  inf_guide: procedure_id * exp list;
  inf_input: channel_id * string loc;
  inf_output: channel_id * string loc;
}
