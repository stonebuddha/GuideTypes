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

type subroutine = {
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
  sys_output_channel: string;
  sys_output_filename: string;
}
