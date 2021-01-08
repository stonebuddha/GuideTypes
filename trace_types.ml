open Core
open Ast_types
open Value_types

type event =
  | Ev_tensor_left of Tensor.t
  | Ev_tensor_right of Tensor.t
  | Ev_branch_left of bool
  | Ev_branch_right of bool

type trace = event list

type continuation =
  | Cont_stop
  | Cont_bind of string option * cmd * closure * continuation
  | Cont_loop of int * string * cmd * closure * continuation
  | Cont_iter of Tensor.t list * string * string * cmd * closure * continuation

type subroutine = {
  mutable subr_log_prob_sum: Tensor.t;
  mutable subr_env: closure;
  mutable subr_cont: (cmd, value) Either.t * continuation;
  subr_channel_left: string option;
  subr_channel_right: string option;
}

type msg_buffer = event Queue.t

type system = {
  sys_buffer: msg_buffer String.Map.t;
  sys_model: subroutine;
  sys_guide: subroutine;
  sys_input_channel: string;
  sys_output_channel: string;
}

type system_spec = {
  sys_spec_model: cmd * (string * base_tyv * exp option) list * string option * string option;
  sys_spec_guide: cmd * (string * base_tyv * exp option) list * string option * string option;
  sys_spec_input_channel: string;
  sys_spec_input_traces: trace;
  sys_spec_output_channel: string;
  sys_spec_output_filename: string;
}
