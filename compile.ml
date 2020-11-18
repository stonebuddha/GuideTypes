open Core
open Ast_types
open Ir_types

let tab = "    "

let genvar =
  let cnt = ref 0 in
  fun () ->
    let res = "_gensym_" ^ Int.to_string !cnt in
    incr cnt;
    res

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
  | AE_real f -> Format.fprintf fmt "%.8f" f
  | AE_nat n -> Format.fprintf fmt "%d" n
  | AE_binop (bop, exp1, exp2) ->
    Format.fprintf fmt "(%a %a %a)" emit_aexp exp1 emit_bop bop emit_aexp exp2
  | AE_dist dist -> Format.fprintf fmt "%a" emit_dist dist
  | AE_tensor exp0 -> Format.fprintf fmt "torch.tensor(%a)" emit_aexp exp0
  | AE_stack exps -> Format.fprintf fmt "torch.stack((%a))"
                       (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") emit_aexp) exps
  | AE_index (base_exp, index_exps) ->
    Format.fprintf fmt "%a%a.item()" emit_aexp base_exp
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "")
         (fun fmt aexp -> Format.fprintf fmt "[%a]" emit_aexp aexp))
      index_exps
  | AE_pair (exp1, exp2) ->
    Format.fprintf fmt "(%a, %a)" emit_aexp exp1 emit_aexp exp2
  | AE_field (exp0, field) ->
    Format.fprintf fmt "%a[%d]" emit_aexp exp0 field

and emit_dist fmt = function
  | D_ber exp -> Format.fprintf fmt "dist.Bernoulli(%a)" emit_aexp exp
  | D_unif -> Format.fprintf fmt "dist.Uniform(0., 1.)"
  | D_beta (exp1, exp2) -> Format.fprintf fmt "dist.Beta(%a, %a)" emit_aexp exp1 emit_aexp exp2
  | D_gamma (exp1, exp2) -> Format.fprintf fmt "dist.Gamma(%a, %a)" emit_aexp exp1 emit_aexp exp2
  | D_normal (exp1, exp2) -> Format.fprintf fmt "dist.Normal(%a, %a)" emit_aexp exp1 emit_aexp exp2
  | D_cat exps ->
    Format.fprintf fmt "dist.Categorical(torch.tensor([%a]))"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") emit_aexp) exps
  | D_discrete exp -> Format.fprintf fmt "dist.Categorical(%a)" emit_aexp exp
  | D_bin (n, exp) -> Format.fprintf fmt "dist.Binomial(%d, %a)" n emit_aexp exp
  | D_geo exp -> Format.fprintf fmt "dist.Geometric(%a)" emit_aexp exp
  | D_pois exp -> Format.fprintf fmt "dist.Poisson(%a)" emit_aexp exp

let emit_ret_or_bnd ?bind lev fmt =
  match bind with
  | None ->
    Format.fprintf fmt "%sreturn %a@." lev
  | Some (Some var_name) ->
    Format.fprintf fmt "%s%s = %a@." lev var_name
  | Some None ->
    Format.fprintf fmt "%s%a@." lev

let rec emit_cexp ~comm ~extra ?bind lev fmt = function
  | CE_app (exp1, exp2) ->
    emit_ret_or_bnd ?bind lev fmt (fun fmt () -> Format.fprintf fmt "%a(%a)" emit_aexp exp1 emit_aexp exp2) ()
  | CE_call (proc_name, exps) ->
    emit_ret_or_bnd ?bind lev fmt (fun fmt () -> Format.fprintf fmt "self.%s(%a)" proc_name
                                      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") emit_aexp)
                                      exps) ()
  | CE_cond (exp0, exp1, exp2) ->
    Format.fprintf fmt "%sif %a:@.%a%selse:@.%a" lev emit_aexp exp0 (emit_iexp ~comm ~extra ?bind (lev ^ tab)) exp1 lev (emit_iexp ~comm ~extra ?bind (lev ^ tab)) exp2

  | CE_sample_recv (exp0, channel_name) ->
    Format.fprintf fmt "%sself.%s += 1@." lev ("_" ^ channel_name ^ "cnt");
    begin
      match comm with
      | Some (_, comm_r) ->
        emit_ret_or_bnd ?bind lev fmt (fun fmt () ->
            Format.fprintf fmt "helper_%s.switch()" (Map.find_exn comm_r channel_name)
          ) ()
      | None ->
        emit_ret_or_bnd ?bind lev fmt (fun fmt () ->
            Format.fprintf fmt "pyro.sample(\"%s_\" + str(%s), %a)"
              channel_name
              ("self._" ^ channel_name ^ "cnt")
              emit_aexp exp0
          ) ()
    end
  | CE_sample_send (exp0, channel_name) ->
    Format.fprintf fmt "%sself.%s += 1@." lev ("_" ^ channel_name ^ "cnt");
    begin
      match comm with
      | Some (comm_l, _) ->
        let bind = Option.value_exn (Option.value_exn bind) in
        emit_ret_or_bnd ~bind:(Some bind) lev fmt (fun fmt () ->
            Format.fprintf fmt "pyro.sample(\"%s_\" + str(%s), %a)"
              channel_name
              ("self._" ^ channel_name ^ "cnt")
              emit_aexp exp0
          ) ();
        begin
          match Map.find comm_l channel_name with
          | None -> ()
          | Some proc_name ->
            emit_ret_or_bnd ~bind:None lev fmt (fun fmt () ->
                Format.fprintf fmt "helper_%s.switch(%s)" proc_name bind
              ) ()
        end
      | None ->
        emit_ret_or_bnd ?bind lev fmt (fun fmt () ->
            Format.fprintf fmt "pyro.sample(\"%s_\" + str(%s), %a)"
              channel_name
              ("self._" ^ channel_name ^ "cnt")
              emit_aexp exp0
          ) ()
    end

  | CE_cond_recv (exp1, exp2, channel_name) ->
    begin
      match comm with
      | None ->
        Format.fprintf fmt "%sif yield:@.%a%selse:@.%a" lev (emit_iexp ~comm ~extra ?bind (lev ^ tab)) exp1 lev (emit_iexp ~comm ~extra ?bind (lev ^ tab)) exp2
      | Some (_, comm_r) ->
        Format.fprintf fmt "%sif helper_%s.switch():@.%a%selse:@.%a"
          lev (Map.find_exn comm_r channel_name)
          (emit_iexp ~comm ~extra ?bind (lev ^ tab)) exp1
          lev
          (emit_iexp ~comm ~extra ?bind (lev ^ tab)) exp2
    end

  | CE_cond_send (exp0, exp1, exp2, channel_name) ->
    begin
      match comm with
      | None ->
        Format.fprintf fmt "%sif %a:@.%a%selse:@.%a" lev emit_aexp exp0 (emit_iexp ~comm ~extra ?bind (lev ^ tab)) exp1 lev (emit_iexp ~comm ~extra ?bind (lev ^ tab)) exp2
      | Some (comm_l, _) ->
        match Map.find comm_l channel_name with
        | None -> Format.fprintf fmt "%sif %a:@.%a%selse:@.%a" lev emit_aexp exp0 (emit_iexp ~comm ~extra ?bind (lev ^ tab)) exp1 lev (emit_iexp ~comm ~extra ?bind (lev ^ tab)) exp2
        | Some proc_name ->
          Format.fprintf fmt "%sif %a:@.%shelper_%s.switch(True)@.%a%selse:@.%shelper_%s.switch(False)@.%a"
            lev emit_aexp exp0
            (lev ^ tab) proc_name
            (emit_iexp ~comm ~extra ?bind (lev ^ tab)) exp1
            lev
            (lev ^ tab) proc_name
            (emit_iexp ~comm ~extra ?bind (lev ^ tab)) exp2
    end

  | CE_iter (exp1, exp2, iter_name, bind_name, exp3) ->
    Format.fprintf fmt "%s%s = %a@." lev bind_name emit_aexp exp2;
    Format.fprintf fmt "%sfor %s in %a:@." lev iter_name emit_aexp exp1;
    Format.fprintf fmt "%a" (emit_iexp ~comm ~extra ~bind:(Some bind_name) (lev ^ tab)) exp3;
    begin
      match bind with
      | Some None -> ()
      | _ -> emit_ret_or_bnd ?bind lev fmt (fun fmt () -> Format.fprintf fmt "%s" bind_name) ()
    end

  | CE_loop (n, exp1, bind_name, exp2) ->
    Format.fprintf fmt "%s%s = %a@." lev bind_name emit_aexp exp1;
    Format.fprintf fmt "%sfor _ in range(%d):@." lev n;
    Format.fprintf fmt "%a" (emit_iexp ~comm ~extra ~bind:(Some bind_name) (lev ^ tab)) exp2;
    begin
      match bind with
      | Some None -> ()
      | _ -> emit_ret_or_bnd ?bind lev fmt (fun fmt () -> Format.fprintf fmt "%s" bind_name) ()
    end

  | CE_abs (var_name, body_exp) ->
    begin
      match bind with
      | Some None -> ()
      | _ ->
        begin
          let lambda_name = genvar () in
          Format.fprintf fmt "%sdef %s(%s):@." lev lambda_name var_name;
          Format.fprintf fmt "%a" (emit_iexp ~comm ~extra (lev ^ tab)) body_exp;
          emit_ret_or_bnd ?bind lev fmt (fun fmt () -> Format.fprintf fmt "%s" lambda_name) ()
        end
    end

and emit_aexp_or_cexp ~comm ~extra ?bind lev fmt =
  Either.value_map ~first:(emit_ret_or_bnd ?bind lev fmt emit_aexp) ~second:(emit_cexp ~comm ~extra ?bind lev fmt)

and emit_iexp ~comm ~extra ?bind lev fmt = function
  | IE_tail exp -> emit_aexp_or_cexp ~comm ~extra ?bind lev fmt exp
  | IE_let (exp1, var_name, exp2) ->
    emit_aexp_or_cexp ~comm ~extra ~bind:var_name lev fmt exp1;
    emit_iexp ~comm ~extra ?bind lev fmt exp2

let emit_proc ~comm lev fmt (proc_name, proc) =
  let extra = List.map
      (List.append (Option.to_list proc.iproc_sig.ipsig_sess_left) (Option.to_list proc.iproc_sig.ipsig_sess_right))
      ~f:(fun channel_name -> "_" ^ channel_name ^ "cnt")
  in
  Format.fprintf fmt "%sdef %s(self%a):@."
    lev
    proc_name
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "") (fun fmt s -> Format.fprintf fmt ", %s" s)) proc.iproc_sig.ipsig_params;
  Format.fprintf fmt "%a"
    (emit_iexp ~comm ~extra (lev ^ tab))
    proc.iproc_body

let emit_prog_for_model fmt prog =
  let (model_proc_name, model_proc) = List.hd_exn prog in
  Format.fprintf fmt
    "class Wrapper_for_%s:@."
    model_proc_name;
  Format.fprintf fmt
    "%sdef run(self%a):@."
    tab
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "") (fun fmt s -> Format.fprintf fmt ", %s" s)) (model_proc.iproc_sig.ipsig_params);
  List.iter
    (List.append (Option.to_list model_proc.iproc_sig.ipsig_sess_left) (Option.to_list model_proc.iproc_sig.ipsig_sess_right))
    ~f:(fun channel_name ->
        let field_name = "_" ^ channel_name ^ "cnt" in
        Format.fprintf fmt "%sself.%s = 0@." (tab ^ tab) field_name);
  Format.fprintf fmt "%sreturn self.%s(%a)@." (tab ^ tab) model_proc_name
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") (fun fmt s -> Format.fprintf fmt ", %s" s)) (model_proc.iproc_sig.ipsig_params);
  List.iter prog ~f:(fun top -> Format.fprintf fmt "@.%a" (emit_proc ~comm:None tab) top)

(* let emit_prog_for_guide fmt prog =
   let (model_proc_name, model_proc) = List.hd_exn prog in
   Format.fprintf fmt
    "def Importance_for_%s(%a):@."
    model_proc_name
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") String.pp) (model_proc.iproc_sig.ipsig_params);
   let comm_l = String.Map.of_alist_exn (List.filter_map prog ~f:(fun (proc_name, proc) -> Option.map proc.iproc_sig.ipsig_sess_left ~f:(fun channel_name -> (channel_name, proc_name)))) in
   let comm_r = String.Map.of_alist_exn (List.filter_map prog ~f:(fun (proc_name, proc) -> Option.map proc.iproc_sig.ipsig_sess_right ~f:(fun channel_name -> (channel_name, proc_name)))) in
   List.iter prog ~f:(fun top -> Format.fprintf fmt "@.%a" (emit_proc ~comm:(Some (comm_l, comm_r)) tab) top);
   Format.fprintf fmt "@.";
   List.iter prog ~f:(fun (proc_name, _) -> Format.fprintf fmt "%shelper_%s = greenlet(%s)@." tab proc_name proc_name);
   Format.fprintf fmt "@.";
   Format.fprintf fmt "%shelper_%s.switch()@." tab model_proc_name *)

(* let emit_prog_for_importance fmt prog =
   emit_prog_for_model fmt prog;
   Format.fprintf fmt "@.";
   emit_prog_for_guide fmt prog *)
