val implementation : Lexing.lexbuf -> (Ast_types.prog * Infer_types.script option) Core.Or_error.t

val batch_traces : Lexing.lexbuf -> Trace_types.trace list Core.Or_error.t
