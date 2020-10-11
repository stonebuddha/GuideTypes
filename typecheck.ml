open Core
open Ast_types

exception Type_error of string * Location.t

let is_prim_numeric = function
  | Pty_ureal
  | Pty_preal
  | Pty_real
  | Pty_fnat _
  | Pty_nat -> true
  | _ -> false

let is_prim_subtype pty1 pty2 =
  match pty1, pty2 with
  | Pty_unit, Pty_unit
  | Pty_bool, Pty_bool
  | Pty_ureal, Pty_ureal
  | Pty_ureal, Pty_preal
  | Pty_ureal, Pty_real
  | Pty_preal, Pty_preal
  | Pty_preal, Pty_real
  | Pty_real, Pty_real
  | Pty_fnat _, Pty_nat
  | Pty_nat, Pty_nat -> true
  | Pty_fnat n, Pty_fnat m -> n <= m
  | _ -> false

let rec is_subtype tyv1 tyv2 =
  match tyv1, tyv2 with
  | Btyv_prim pty1, Btyv_prim pty2 -> is_prim_subtype pty1 pty2
  | Btyv_dist tyv1', Btyv_dist tyv2' -> equal_base_tyv tyv1' tyv2'
  | Btyv_arrow (tyv11, tyv12), Btyv_arrow (tyv21, tyv22) -> is_subtype tyv21 tyv11 && is_subtype tyv12 tyv22
  | _ -> false

let join_prim ~loc pty1 pty2 =
  if is_prim_subtype pty1 pty2 then
    Ok pty2
  else if is_prim_subtype pty2 pty1 then
    Ok pty1
  else
    Error (Type_error ("join error", loc))

let meet_prim ~loc pty1 pty2 =
  if is_prim_subtype pty1 pty2 then
    Ok pty1
  else if is_prim_subtype pty2 pty1 then
    Ok pty2
  else
    Error (Type_error ("meet error", loc))

let rec join_type ~loc tyv1 tyv2 =
  let open Result.Let_syntax in
  match tyv1, tyv2 with
  | Btyv_prim pty1, Btyv_prim pty2 ->
    let%bind pty = join_prim ~loc pty1 pty2 in
    Ok (Btyv_prim pty)
  | Btyv_dist tyv1', Btyv_dist tyv2' ->
    if equal_base_tyv tyv1' tyv2' then
      Ok (Btyv_dist tyv1')
    else
      Error (Type_error ("join error", loc))
  | Btyv_arrow (tyv11, tyv12), Btyv_arrow (tyv21, tyv22) ->
    let%bind tyv1' = meet_type ~loc tyv11 tyv21 in
    let%bind tyv2' = join_type ~loc tyv12 tyv22 in
    Ok (Btyv_arrow (tyv1', tyv2'))
  | _ -> Error (Type_error ("join error", loc))

and meet_type ~loc tyv1 tyv2 =
  let open Result.Let_syntax in
  match tyv1, tyv2 with
  | Btyv_prim pty1, Btyv_prim pty2 ->
    let%bind pty = meet_prim ~loc pty1 pty2 in
    Ok (Btyv_prim pty)
  | Btyv_dist tyv1', Btyv_dist tyv2' ->
    if equal_base_tyv tyv1' tyv2' then
      Ok (Btyv_dist tyv1')
    else
      Error (Type_error ("meet error", loc))
  | Btyv_arrow (tyv11, tyv12), Btyv_arrow (tyv21, tyv22) ->
    let%bind tyv1' = join_type ~loc tyv11 tyv21 in
    let%bind tyv2' = meet_type ~loc tyv12 tyv22 in
    Ok (Btyv_arrow (tyv1', tyv2'))
  | _ -> Error (Type_error ("meet error", loc))

let rec eval_bty ty =
  match ty.bty_desc with
  | Bty_prim pty -> Btyv_prim pty
  | Bty_arrow (ty1, ty2) -> Btyv_arrow (eval_bty ty1, eval_bty ty2)
  | Bty_dist ty0 -> Btyv_dist (eval_bty ty0)

let tycheck_bop bop arg1 arg2 =
  let open Result.Let_syntax in
  match arg1, arg2 with
  | Btyv_prim pty1, Btyv_prim pty2 ->
    let%bind res =
      match bop.txt, pty1, pty2 with
      | Bop_add, Pty_ureal, Pty_ureal
      | Bop_add, Pty_ureal, Pty_preal -> Ok Pty_preal
      | Bop_add, Pty_ureal, Pty_real -> Ok Pty_real
      | Bop_add, Pty_preal, Pty_ureal
      | Bop_add, Pty_preal, Pty_preal -> Ok Pty_preal
      | Bop_add, Pty_preal, Pty_real -> Ok Pty_real
      | Bop_add, Pty_real, Pty_ureal
      | Bop_add, Pty_real, Pty_preal
      | Bop_add, Pty_real, Pty_real -> Ok Pty_real
      | Bop_add, Pty_fnat n, Pty_fnat m -> Ok (Pty_fnat (n + m))
      | Bop_add, Pty_fnat _, Pty_nat -> Ok Pty_nat
      | Bop_add, Pty_nat, Pty_fnat _
      | Bop_add, Pty_nat, Pty_nat -> Ok Pty_nat

      | Bop_sub, Pty_ureal, Pty_ureal
      | Bop_sub, Pty_ureal, Pty_preal
      | Bop_sub, Pty_ureal, Pty_real
      | Bop_sub, Pty_preal, Pty_ureal
      | Bop_sub, Pty_preal, Pty_preal
      | Bop_sub, Pty_preal, Pty_real
      | Bop_sub, Pty_real, Pty_ureal
      | Bop_sub, Pty_real, Pty_preal
      | Bop_sub, Pty_real, Pty_real -> Ok Pty_real

      | Bop_mul, Pty_ureal, Pty_ureal -> Ok Pty_ureal
      | Bop_mul, Pty_ureal, Pty_preal -> Ok Pty_preal
      | Bop_mul, Pty_ureal, Pty_real -> Ok Pty_real
      | Bop_mul, Pty_preal, Pty_ureal -> Ok Pty_preal
      | Bop_mul, Pty_preal, Pty_preal -> Ok Pty_preal
      | Bop_mul, Pty_preal, Pty_real -> Ok Pty_real
      | Bop_mul, Pty_real, Pty_ureal -> Ok Pty_real
      | Bop_mul, Pty_real, Pty_preal -> Ok Pty_real
      | Bop_mul, Pty_real, Pty_real -> Ok Pty_real

      | Bop_div, Pty_ureal, Pty_ureal -> Ok Pty_preal
      | Bop_div, Pty_ureal, Pty_preal -> Ok Pty_preal
      | Bop_div, Pty_ureal, Pty_real -> Ok Pty_real
      | Bop_div, Pty_preal, Pty_ureal -> Ok Pty_preal
      | Bop_div, Pty_preal, Pty_preal -> Ok Pty_preal
      | Bop_div, Pty_preal, Pty_real -> Ok Pty_real
      | Bop_div, Pty_real, Pty_ureal -> Ok Pty_real
      | Bop_div, Pty_real, Pty_preal -> Ok Pty_real
      | Bop_div, Pty_real, Pty_real -> Ok Pty_real

      | Bop_eq, pty1, pty2
      | Bop_ne, pty1, pty2 when is_prim_subtype pty1 pty2 || is_prim_subtype pty2 pty1 -> Ok Pty_bool

      | Bop_lt, pty1, pty2
      | Bop_le, pty1, pty2
      | Bop_gt, pty1, pty2
      | Bop_ge, pty1, pty2 when is_prim_numeric pty1 && (is_prim_subtype pty1 pty2 || is_prim_subtype pty2 pty1) -> Ok Pty_bool

      | Bop_and, Pty_bool, Pty_bool
      | Bop_or, Pty_bool, Pty_bool -> Ok Pty_bool

      | _ -> Error (Type_error ("mismatched operand types", bop.loc))
    in
    Ok (Btyv_prim res)
  | _ -> Error (Type_error ("mismatched operand types", bop.loc))

let rec tycheck_exp ctxt exp =
  let open Result.Let_syntax in
  match exp.exp_desc with
  | E_var var_name ->
    begin
      match Map.find ctxt var_name.txt with
      | Some tyv -> Ok tyv
      | None -> Error (Type_error ("undefined variable " ^ var_name.txt, exp.exp_loc))
    end
  | E_triv -> Ok (Btyv_prim Pty_unit)
  | E_bool _ -> Ok (Btyv_prim Pty_bool)
  | E_cond (exp0, exp1, exp2) ->
    let%bind tyv0 = tycheck_exp ctxt exp0 in
    if is_subtype tyv0 (Btyv_prim Pty_bool) then
      let%bind tyv1 = tycheck_exp ctxt exp1 in
      let%bind tyv2 = tycheck_exp ctxt exp2 in
      join_type ~loc:exp.exp_loc tyv1 tyv2
    else
      Error (Type_error ("non-boolean condition type", exp0.exp_loc))
  | E_real r ->
    if Float.(r > 0. && r < 1.) then
      Ok (Btyv_prim Pty_ureal)
    else if Float.(r > 0.) then
      Ok (Btyv_prim Pty_preal)
    else
      Ok (Btyv_prim Pty_real)
  | E_nat n ->
    if n >= 0 then
      Ok (Btyv_prim (Pty_fnat (n + 1)))
    else
      Error (Type_error ("negative integers", exp.exp_loc))
  | E_binop (bop, exp1, exp2) ->
    let%bind tyv1 = tycheck_exp ctxt exp1 in
    let%bind tyv2 = tycheck_exp ctxt exp2 in
    tycheck_bop bop tyv1 tyv2
  | E_abs (var_name, ty, exp0) ->
    let tyv = eval_bty ty in
    let%bind tyv0 = tycheck_exp (Map.set ctxt ~key:var_name.txt ~data:tyv) exp0 in
    Ok (Btyv_arrow (tyv, tyv0))
  | E_app (exp1, exp2) ->
    let%bind tyv1 = tycheck_exp ctxt exp1 in
    let%bind tyv2 = tycheck_exp ctxt exp2 in
    begin
      match tyv1 with
      | Btyv_arrow (tyv11, tyv12) ->
        if is_subtype tyv2 tyv11 then
          Ok tyv12
        else
          Error (Type_error ("mismatched argument types", exp2.exp_loc))
      | _ ->
        Error (Type_error ("non-arrow function type", exp.exp_loc))
    end
  | E_let (exp1, var_name, exp2) ->
    let%bind tyv1 = tycheck_exp ctxt exp1 in
    tycheck_exp (Map.set ctxt ~key:var_name.txt ~data:tyv1) exp2
  | E_dist dist ->
    let%bind tyv = tycheck_dist ~loc:exp.exp_loc ctxt dist in
    Ok (Btyv_dist tyv)

and tycheck_dist ~loc ctxt dist =
  let open Result.Let_syntax in
  match dist with
  | D_ber exp ->
    let%bind tyv = tycheck_exp ctxt exp in
    if is_subtype tyv (Btyv_prim Pty_ureal) then
      Ok (Btyv_prim Pty_bool)
    else
      Error (Type_error ("mismatched parameter types", loc))
  | D_unif ->
    Ok (Btyv_prim Pty_ureal)
  | D_beta (exp1, exp2) ->
    let%bind tyv1 = tycheck_exp ctxt exp1 in
    let%bind tyv2 = tycheck_exp ctxt exp2 in
    if is_subtype tyv1 (Btyv_prim Pty_preal) && is_subtype tyv2 (Btyv_prim Pty_preal) then
      Ok (Btyv_prim Pty_ureal)
    else
      Error (Type_error ("mismatched parameter types", loc))
  | D_gamma (exp1, exp2) ->
    let%bind tyv1 = tycheck_exp ctxt exp1 in
    let%bind tyv2 = tycheck_exp ctxt exp2 in
    if is_subtype tyv1 (Btyv_prim Pty_preal) && is_subtype tyv2 (Btyv_prim Pty_preal) then
      Ok (Btyv_prim Pty_preal)
    else
      Error (Type_error ("mismatched parameter types", loc))
  | D_normal (exp1, exp2) ->
    let%bind tyv1 = tycheck_exp ctxt exp1 in
    let%bind tyv2 = tycheck_exp ctxt exp2 in
    if is_subtype tyv1 (Btyv_prim Pty_real) && is_subtype tyv2 (Btyv_prim Pty_preal) then
      Ok (Btyv_prim Pty_real)
    else
      Error (Type_error ("mismatched parameter types", loc))
  | D_cat exps ->
    let n = List.length exps in
    let%bind () = List.fold_result exps ~init:() ~f:(fun () exp ->
        let%bind tyv = tycheck_exp ctxt exp in
        if is_subtype tyv (Btyv_prim Pty_preal) then
          Ok ()
        else
          Error (Type_error ("mismatched parameter types", loc))
      )
    in
    Ok (Btyv_prim (Pty_fnat n))
  | D_geo exp ->
    let%bind tyv = tycheck_exp ctxt exp in
    if is_subtype tyv (Btyv_prim Pty_ureal) then
      Ok (Btyv_prim Pty_nat)
    else
      Error (Type_error ("mismatched parameter types", loc))

let rec eval_sty sty =
  match sty.sty_desc with
  | Sty_one -> Styv_one
  | Sty_conj (ty1, sty2) -> Styv_conj (eval_bty ty1, eval_sty sty2)
  | Sty_imply (ty1, sty2) -> Styv_imply (eval_bty ty1, eval_sty sty2)
  | Sty_ichoice (sty1, sty2) -> Styv_ichoice (eval_sty sty1, eval_sty sty2)
  | Sty_echoice (sty1, sty2) -> Styv_echoice (eval_sty sty1, eval_sty sty2)
  | Sty_var type_name -> Styv_var type_name.txt

let collect_sess_tys prog =
  String.Map.of_alist_exn (List.filter_map prog ~f:(fun top ->
      match top with
      | Top_proc _ -> None
      | Top_sess (type_name, sty) -> Some (type_name.txt, eval_sty sty)
    ))

let eval_proc_sig psig =
  { psigv_param_tys = List.map psig.psig_param_tys ~f:(fun (var_name, ty) -> (var_name.txt, eval_bty ty))
  ; psigv_ret_ty = eval_bty psig.psig_ret_ty
  ; psigv_sess_left = Option.map psig.psig_sess_left ~f:(fun (channel_name, type_name) -> (channel_name.txt, type_name.txt))
  ; psigv_sess_right = Option.map psig.psig_sess_right ~f:(fun (channel_name, type_name) -> (channel_name.txt, type_name.txt))
  }

let collect_proc_sigs prog =
  String.Map.of_alist_exn (List.filter_map prog ~f:(fun top ->
      match top with
      | Top_sess _ -> None
      | Top_proc (proc_name, { proc_sig; _ }) -> Some (proc_name.txt, eval_proc_sig proc_sig)
    ))

let rec continualize sty = function
  | Styv_one -> sty
  | Styv_conj (tyv1, sty2) -> Styv_conj (tyv1, continualize sty sty2)
  | Styv_imply (tyv1, sty2) -> Styv_imply (tyv1, continualize sty sty2)
  | Styv_ichoice (sty1, sty2) -> Styv_ichoice (continualize sty sty1, continualize sty sty2)
  | Styv_echoice (sty1, sty2) -> Styv_echoice (continualize sty sty1, continualize sty sty2)
  | Styv_var id -> Styv_var id

let tycheck_cmd sty_ctxt psig_ctxt =
  let open Result.Let_syntax in
  let rec forward ctxt cmd =
    match cmd.cmd_desc with
    | M_ret exp ->
      tycheck_exp ctxt exp
    | M_bnd (cmd1, var_name, cmd2) ->
      let %bind tyv1 = forward ctxt cmd1 in
      forward (Map.set ctxt ~key:var_name.txt ~data:tyv1) cmd2
    | M_call (proc_name, exps) ->
      begin
        match Map.find psig_ctxt proc_name.txt with
        | None -> Error (Type_error ("unknown procedure " ^ proc_name.txt, proc_name.loc))
        | Some psigv ->
          if List.length psigv.psigv_param_tys <> List.length exps then
            Error (Type_error ("mismatched arity", cmd.cmd_loc))
          else
            let%bind tyvs = List.fold_result (List.rev exps) ~init:[] ~f:(fun acc exp ->
                let%bind tyv = tycheck_exp ctxt exp in
                Ok (tyv :: acc)
              )
            in
            if not (List.for_all2_exn tyvs psigv.psigv_param_tys ~f:(fun tyv (_, tyv') -> is_subtype tyv tyv')) then
              Error (Type_error ("mismatched argument types", cmd.cmd_loc))
            else
              Ok psigv.psigv_ret_ty
      end
    | M_sample_recv (exp, _)
    | M_sample_send (exp, _) ->
      let%bind tyv = tycheck_exp ctxt exp in
      begin
        match tyv with
        | Btyv_dist tyv0 -> Ok tyv0
        | _ -> Error (Type_error ("non-distribution types", exp.exp_loc))
      end
    | M_branch_recv (cmd1, cmd2, _) ->
      let%bind tyv1 = forward ctxt cmd1 in
      let%bind tyv2 = forward ctxt cmd2 in
      join_type ~loc:cmd.cmd_loc tyv1 tyv2
    | M_branch_send (exp, cmd1, cmd2, _) ->
      let%bind tyv = tycheck_exp ctxt exp in
      begin
        match tyv with
        | Btyv_prim Pty_bool ->
          let%bind tyv1 = forward ctxt cmd1 in
          let%bind tyv2 = forward ctxt cmd2 in
          join_type ~loc:cmd.cmd_loc tyv1 tyv2
        | _ ->
          Error (Type_error ("non-boolean condition type", exp.exp_loc))
      end
  in
  let rec backward ctxt sess cmd =
    match cmd.cmd_desc with
    | M_ret _ ->
      Ok sess
    | M_bnd (cmd1, var_name, cmd2) ->
      let%bind tyv1 = forward ctxt cmd1 in
      let%bind sess' = backward (Map.set ctxt ~key:var_name.txt ~data:tyv1) sess cmd2 in
      backward ctxt sess' cmd1
    | M_sample_recv (exp, channel_name) ->
      let%bind tyv = tycheck_exp ctxt exp in
      begin
        match Map.find sess channel_name.txt with
        | None -> Error (Type_error ("unknown channel " ^ channel_name.txt, channel_name.loc))
        | Some (`Left, sty) ->
          Ok (Map.set sess ~key:channel_name.txt ~data:(`Left, Styv_conj (tyv, sty)))
        | Some (`Right, sty) ->
          Ok (Map.set sess ~key:channel_name.txt ~data:(`Right, Styv_imply (tyv, sty)))
      end
    | M_sample_send (exp, channel_name) ->
      let%bind tyv = tycheck_exp ctxt exp in
      begin
        match Map.find sess channel_name.txt with
        | None -> Error (Type_error ("unknown channel " ^ channel_name.txt, channel_name.loc))
        | Some (`Left, sty) ->
          Ok (Map.set sess ~key:channel_name.txt ~data:(`Left, Styv_imply (tyv, sty)))
        | Some (`Right, sty) ->
          Ok (Map.set sess ~key:channel_name.txt ~data:(`Right, Styv_conj (tyv, sty)))
      end
    | M_branch_recv (cmd1, cmd2, channel_name) ->
      let%bind sess1 = backward ctxt sess cmd1 in
      let%bind sess2 = backward ctxt sess cmd2 in
      Result.try_with (fun () ->
          Map.merge sess1 sess2 ~f:(fun ~key -> function
              | `Left _
              | `Right _ -> assert false
              | `Both ((dir1, sty1), (_, sty2)) ->
                if String.(key = channel_name.txt) then
                  match dir1 with
                  | `Left -> Some (`Left, Styv_ichoice (sty1, sty2))
                  | `Right -> Some (`Right, Styv_echoice (sty1, sty2))
                else if equal_sess_tyv sty1 sty2 then
                  Some (dir1, sty1)
                else
                  raise (Type_error ("mismatched sessions", cmd.cmd_loc))
            )
        )
    | M_branch_send (_, cmd1, cmd2, channel_name) ->
      let%bind sess1 = backward ctxt sess cmd1 in
      let%bind sess2 = backward ctxt sess cmd2 in
      Result.try_with (fun () ->
          Map.merge sess1 sess2 ~f:(fun ~key -> function
              | `Left _
              | `Right _ -> assert false
              | `Both ((dir1, sty1), (_, sty2)) ->
                if String.(key = channel_name.txt) then
                  match dir1 with
                  | `Left -> Some (`Left, Styv_echoice (sty1, sty2))
                  | `Right -> Some (`Right, Styv_ichoice (sty1, sty2))
                else if equal_sess_tyv sty1 sty2 then
                  Some (dir1, sty1)
                else
                  raise (Type_error ("mismatched sessions", cmd.cmd_loc))
            )
        )
    | M_call (proc_name, _) ->
      begin
        match Map.find psig_ctxt proc_name.txt with
        | None -> Error (Type_error ("unknown procedure " ^ proc_name.txt, proc_name.loc))
        | Some psigv ->
          let sess0 = String.Map.of_alist_exn
              (List.append (Option.to_list psigv.psigv_sess_left) (Option.to_list psigv.psigv_sess_right)) in
          if not (Set.equal (Map.key_set sess0) (Map.key_set sess)) then
            Error (Type_error ("mismatched channels", cmd.cmd_loc))
          else
            Result.try_with (fun () ->
                Map.merge sess0 sess ~f:(fun ~key:_ -> function
                    | `Left _
                    | `Right _ -> assert false
                    | `Both (type_id, (dir, sty)) ->
                      match Map.find sty_ctxt type_id with
                      | None -> raise (Type_error ("unknown type name " ^ type_id, cmd.cmd_loc))
                      | Some sty_def ->
                        Some (dir, (continualize sty sty_def))
                  )
              )
      end
  in
  fun ctxt sess_left sess_right cmd ->
    let%bind tyv = forward ctxt cmd in
    let sess_left = Option.map sess_left ~f:(fun (k, v) -> (k, (`Left, v))) in
    let sess_right = Option.map sess_right ~f:(fun (k, v) -> (k, (`Right, v))) in
    let sess = String.Map.of_alist_exn (List.append (Option.to_list sess_left) (Option.to_list sess_right)) in
    let%bind sess' = backward ctxt sess cmd in
    Ok (tyv,
        Option.map sess_left ~f:(fun (channel_id, _) -> let (_, sty) = Map.find_exn sess' channel_id in (channel_id, sty)),
        Option.map sess_right ~f:(fun (channel_id, _) -> let (_, sty) = Map.find_exn sess' channel_id in (channel_id, sty))
       )

let tycheck_proc sty_ctxt psig_ctxt proc =
  let open Result.Let_syntax in
  let psigv = eval_proc_sig proc.proc_sig in
  let ctxt = String.Map.of_alist_exn psigv.psigv_param_tys in
  let%bind (tyv, sess_left, sess_right) = tycheck_cmd sty_ctxt psig_ctxt ctxt
      (Option.map psigv.psigv_sess_left ~f:(fun (channel_id, _) -> (channel_id, Styv_one)))
      (Option.map psigv.psigv_sess_right ~f:(fun (channel_id, _) -> (channel_id, Styv_one)))
      proc.proc_body in
  if not (is_subtype tyv psigv.psigv_ret_ty) then
    Error (Type_error ("mismatched signature types", proc.proc_loc))
  else if Option.value_map sess_left ~default:false ~f:(fun (_, sty) ->
      let type_id = Option.value_exn psigv.psigv_sess_left |> snd in
      match Map.find sty_ctxt type_id with
      | None -> true
      | Some sty_def -> not (equal_sess_tyv sty (Styv_var type_id) || equal_sess_tyv sty sty_def)
    ) then
    Error (Type_error ("mismatched left session", proc.proc_loc))
  else if Option.value_map sess_right ~default:false ~f:(fun (_, sty) ->
      let type_id = Option.value_exn psigv.psigv_sess_right |> snd in
      match Map.find sty_ctxt type_id with
      | None -> true
      | Some sty_def -> not (equal_sess_tyv sty (Styv_var type_id) || equal_sess_tyv sty sty_def)
    ) then
    Error (Type_error ("mismatched right session", proc.proc_loc))
  else
    Ok ()

let tycheck_prog prog =
  let sty_ctxt = collect_sess_tys prog in
  let psig_ctxt = collect_proc_sigs prog in
  List.fold_result prog ~init:() ~f:(fun () top ->
      match top with
      | Top_sess _ -> Ok ()
      | Top_proc (_, proc) -> tycheck_proc sty_ctxt psig_ctxt proc
    )

let () =
  Location.register_error_of_exn
    (function
      | Type_error (msg, loc) -> Some (Location.errorf ~loc "%s" msg)
      | _ -> None
    )
