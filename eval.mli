open Core

val infer_system :
  Infer_types.algo ->
  (Ast_types.proc_sigv * Ast_types.cmd) String.Map.t ->
  ((string * Ast_types.base_tyv) list * Ast_types.exp) String.Map.t -> 
  Trace_types.system_spec ->
  unit Or_error.t
