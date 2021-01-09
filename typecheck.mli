open Core

val tycheck_prog :
  Ast_types.prog * Infer_types.script option ->
  ((Ast_types.proc_sigv * Ast_types.cmd) String.Map.t *
   ((string * Ast_types.base_tyv) list * Ast_types.exp) String.Map.t *
   (Trace_types.system_spec * Infer_types.algo) option) Or_error.t
