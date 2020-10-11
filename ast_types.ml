open Core

type 'a loc = 'a Location.loc = {
  txt: 'a;
  loc: Location.t;
}

type variable_id = string loc
type procedure_id = string loc
type channel_id = string loc
type type_id = string loc

type base_ty = {
  bty_desc: base_ty_desc;
  bty_loc: Location.t;
}

and base_ty_desc =
  | Bty_unit
  | Bty_bool
  | Bty_ureal
  | Bty_preal
  | Bty_real
  | Bty_fnat of int
  | Bty_nat
  | Bty_arrow of base_ty * base_ty
  | Bty_dist of base_ty

type binop =
  | Bop_add
  | Bop_sub
  | Bop_mul
  | Bop_div
  | Bop_eq
  | Bop_ne
  | Bop_lt
  | Bop_le
  | Bop_gt
  | Bop_ge
  | Bop_and
  | Bop_or

type 'a dist =
  | D_ber of 'a
  | D_unif
  | D_beta of 'a * 'a
  | D_gamma of 'a * 'a
  | D_normal of 'a * 'a
  | D_cat of 'a list
  | D_geo of 'a

type exp = {
  exp_desc: exp_desc;
  exp_loc: Location.t;
}

and exp_desc =
  | E_var of variable_id
  | E_triv
  | E_bool of bool
  | E_cond of exp * exp * exp
  | E_real of float
  | E_nat of int
  | E_binop of binop loc * exp * exp
  | E_abs of variable_id * base_ty * exp
  | E_app of exp * exp
  | E_let of exp * variable_id * exp
  | E_dist of exp dist

type value =
  | V_triv
  | V_bool of bool
  | V_real of float
  | V_nat of int
  | V_clo of environment * variable_id * base_ty * exp
  | V_dist of value dist

and environment = value String.Map.t

type cmd = {
  cmd_desc: cmd_desc;
  cmd_loc: Location.t;
}

and cmd_desc =
  | M_ret of exp
  | M_bnd of cmd * variable_id * cmd
  | M_call of procedure_id * exp
  | M_sample_recv of exp * channel_id
  | M_sample_send of exp * channel_id
  | M_branch_recv of cmd * cmd * channel_id
  | M_branch_send of exp * cmd * cmd * channel_id

type sess_ty = {
  sty_desc: sess_ty_desc;
  sty_loc: Location.t;
}

and sess_ty_desc =
  | Sty_one
  | Sty_conj of base_ty * sess_ty
  | Sty_imply of base_ty * sess_ty
  | Sty_ichoice of sess_ty * sess_ty
  | Sty_echoice of sess_ty * sess_ty
  | Sty_var of type_id

type proc_sig = {
  psig_param_tys: (variable_id * base_ty) list;
  psig_ret_ty: base_ty;
  psig_sess_left: (channel_id * type_id) option;
  psig_sess_right: (channel_id * type_id) option;
}

type proc = {
  proc_sig: proc_sig;
  proc_body: cmd;
  proc_loc: Location.t;
}

type sess_context = sess_ty String.Map.t

type base_context = base_ty String.Map.t

type proc_context = proc_sig String.Map.t

type proc_environment = proc String.Map.t

type sess_or_proc =
  | Top_sess of type_id * sess_ty
  | Top_proc of procedure_id * proc

type prog = sess_or_proc list
