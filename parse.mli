val implementation : Lexing.lexbuf -> (Ast_types.prog * Infer_types.script option) Core.Or_error.t

val batch_traces : Lexing.lexbuf -> Trace_types.trace Ast_types.loc list Core.Or_error.t

val single_tensor : Lexing.lexbuf -> Tensor.t Ast_types.loc Core.Or_error.t
