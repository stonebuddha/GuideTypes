open Core

val implementation : Lexing.lexbuf -> (Ast_types.prog * Infer_types.script option) Or_error.t
