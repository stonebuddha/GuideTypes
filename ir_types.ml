type aexp =
  | AE_var of string
  | AE_triv
  | AE_bool of bool
  | AE_real of float
  | AE_nat of int
  | AE_binop of Ast_types.binop * aexp * aexp
  | AE_abs of string * iexp
  | AE_dist of aexp Ast_types.dist

and cexp =
  | CE_app of aexp * aexp
  | CE_call of string * aexp list
  | CE_cond of aexp * iexp * iexp
  | CE_cond_recv of iexp * iexp * string
  | CE_cond_send of aexp * iexp * iexp * string
  | CE_sample_recv of aexp * string
  | CE_sample_send of aexp * string

and iexp =
  | IE_let of (aexp, cexp) Core.Either.t * string option * iexp
  | IE_tail of (aexp, cexp) Core.Either.t

type iproc_sig = {
  ipsig_params: string list;
  ipsig_sess_left: string option;
  ipsig_sess_right: string option;
}

type iproc = {
  iproc_sig: iproc_sig;
  iproc_body: iexp;
}

type iprog = (string * iproc) list
