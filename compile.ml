open Core
open Ast_types
open Ir_types

let emit_bop fmt = function
  | Bop_add -> Format.fprintf fmt "+"
  | Bop_sub -> Format.fprintf fmt "-"
  | Bop_mul -> Format.fprintf fmt "*"
  | Bop_div -> Format.fprintf fmt "/"
  | Bop_eq -> Format.fprintf fmt "=="
  | Bop_ne -> Format.fprintf fmt "!="
  | Bop_lt -> Format.fprintf fmt "<"
  | Bop_le -> Format.fprintf fmt "<="
  | Bop_gt -> Format.fprintf fmt ">"
  | Bop_ge -> Format.fprintf fmt ">="
  | Bop_and -> Format.fprintf fmt "and"
  | Bop_or -> Format.fprintf fmt "or"

let rec emit_aexp fmt = function
  | AE_var var_name -> Format.fprintf fmt "%s" var_name
  | AE_triv -> Format.fprintf fmt "()"
  | AE_bool b -> Format.fprintf fmt "%s" (if b then "True" else "False")
  | AE_real f -> Format.fprintf fmt "%a" Float.pp f
  | AE_nat n -> Format.fprintf fmt "%a" Int.pp n
  | AE_binop (bop, exp1, exp2) ->
    Format.fprintf fmt "(%a %a %a)" emit_aexp exp1 emit_bop bop emit_aexp exp2
  | AE_dist dist -> Format.fprintf fmt "%a" emit_dist dist
  | AE_abs _ -> failwith "closure conversion: not implemented"

and emit_dist fmt = function
  | D_ber exp -> Format.fprintf fmt "dist.Bernoull(%a)" emit_aexp exp
  | D_unif -> Format.fprintf fmt "dist.Uniform(0., 1.)"
  | D_beta (exp1, exp2) -> Format.fprintf fmt "dist.Beta(%a, %a)" emit_aexp exp1 emit_aexp exp2
  | D_gamma (exp1, exp2) -> Format.fprintf fmt "dist.Gamma(%a, %a)" emit_aexp exp1 emit_aexp exp2
  | D_normal (exp1, exp2) -> Format.fprintf fmt "dist.Normal(%a, %a)" emit_aexp exp1 emit_aexp exp2
  | D_cat exps ->
    Format.fprintf fmt "dist.Categorical(torch.tensor([%a]))"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") emit_aexp) exps
  | D_geo exp -> Format.fprintf fmt "dist.Geometric(%a)" emit_aexp exp

let emit_ret_or_bnd ?bind lev fmt =
  match bind with
  | None ->
    Format.fprintf fmt "%sreturn %a@." lev
  | Some var_name ->
    Format.fprintf fmt "%s%s = %a@." lev var_name

let rec emit_cexp ?bind lev fmt = function
  | CE_app (exp1, exp2) ->
    emit_ret_or_bnd ?bind lev fmt (fun fmt () -> Format.fprintf fmt "%a(%a)" emit_aexp exp1 emit_aexp exp2) ()
  | CE_call (proc_name, exps) ->
    emit_ret_or_bnd ?bind lev fmt (fun fmt () -> Format.fprintf fmt "%s(%a)" proc_name
                                      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") emit_aexp) exps) ()
  | CE_cond (exp0, exp1, exp2) ->
    Format.fprintf fmt "%sif %a:@.%a%selse:@.%a" lev emit_aexp exp0 (emit_iexp ?bind (lev ^ "\t")) exp1 lev (emit_iexp ?bind (lev ^ "\t")) exp2

  | CE_sample_recv (exp0, _) ->
    emit_ret_or_bnd ?bind lev fmt (fun fmt () -> Format.fprintf fmt "pyro.sample(%a)" emit_aexp exp0) ()
  | CE_sample_send (exp0, _) ->
    emit_ret_or_bnd ?bind lev fmt (fun fmt () -> Format.fprintf fmt "pyro.sample(%a)" emit_aexp exp0) ()

  | CE_cond_recv (exp1, exp2, _) ->
    Format.fprintf fmt "%sif nondet:@.%a%selse:@.%a" lev (emit_iexp ?bind (lev ^ "\t")) exp1 lev (emit_iexp ?bind (lev ^ "\t")) exp2
  | CE_cond_send (exp0, exp1, exp2, _) ->
    Format.fprintf fmt "%sif %a:@.%a%selse:@.%a" lev emit_aexp exp0 (emit_iexp ?bind (lev ^ "\t")) exp1 lev (emit_iexp ?bind (lev ^ "\t")) exp2

and emit_aexp_or_cexp ?bind lev fmt =
  Either.value_map ~first:(emit_ret_or_bnd ?bind lev fmt emit_aexp) ~second:(emit_cexp ?bind lev fmt)

and emit_iexp ?bind lev fmt = function
  | IE_tail exp -> emit_aexp_or_cexp ?bind lev fmt exp
  | IE_let (exp1, var_name, exp2) ->
    emit_aexp_or_cexp ~bind:var_name lev fmt exp1;
    emit_iexp ?bind lev fmt exp2

let emit_proc fmt (proc_name, proc) =
  Format.fprintf fmt
    "def %s(%a):@.%a"
    proc_name
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") String.pp) proc.iproc_sig.ipsig_params
    (emit_iexp "\t") proc.iproc_body

let emit_prog fmt prog =
  Format.fprintf fmt "from greenlet import greenlet@.import torch@.import pyro@.import pyro.distributions as dist@.";
  List.iter prog ~f:(fun top -> Format.fprintf fmt "@.%a" emit_proc top)
