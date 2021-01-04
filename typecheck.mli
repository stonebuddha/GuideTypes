val tycheck_prog : Ast_types.prog * Infer_types.script option -> unit Core.Or_error.t

val tycheck_trace : loc:Location.t -> Ast_types.sess_tyv Core.String.Map.t -> Trace_types.trace -> Ast_types.sess_tyv -> unit Core.Or_error.t
