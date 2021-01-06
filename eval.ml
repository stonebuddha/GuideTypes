open Core
open Ast_types
open Value_types
open Trace_types
open Infer_types
open Or_error.Let_syntax

exception Eval_error of string * Location.t

let stdlib_env = String.Map.of_alist_exn [
    "T", Libtensor.stdlib;
    "D", Libdist.stdlib;
  ]

let bad_impl = Utils.bad_implementation

let lookup_env (libs, cur) lid =
  match lid with
  | Lident_name name ->
    Option.map (Map.find cur name) ~f:(fun value -> Fval_base value)
  | Lident_path (lib_name, name) ->
    begin
      match Map.find libs lib_name with
      | None -> None
      | Some lib -> Map.find lib name
    end

let update_env (libs, cur) ~key ~data =
  (libs, Map.set cur ~key ~data)

let eval_bop bop value1 value2 =
  match bop.txt, value1, value2 with
  | Bop_add, Val_tensor t1, Val_tensor t2 -> Ok (Val_tensor Tensor.(t1 + t2))
  | Bop_sub, Val_tensor t1, Val_tensor t2 -> Ok (Val_tensor Tensor.(t1 - t2))
  | Bop_mul, Val_tensor t1, Val_tensor t2 -> Ok (Val_tensor Tensor.(t1 * t2))
  | Bop_div, Val_tensor t1, Val_tensor t2 -> Ok (Val_tensor Tensor.(t1 / t2))
  | Bop_eq, Val_tensor t1, Val_tensor t2 -> Ok (Val_tensor Tensor.(t1 = t2))
  | Bop_ne, Val_tensor t1, Val_tensor t2 -> Ok (Val_tensor Tensor.(t1 <> t2))
  | Bop_lt, Val_tensor t1, Val_tensor t2 -> Ok (Val_tensor Tensor.(t1 < t2))
  | Bop_le, Val_tensor t1, Val_tensor t2 -> Ok (Val_tensor Tensor.(t1 <= t2))
  | Bop_gt, Val_tensor t1, Val_tensor t2 -> Ok (Val_tensor Tensor.(t1 > t2))
  | Bop_ge, Val_tensor t1, Val_tensor t2 -> Ok (Val_tensor Tensor.(t1 >= t2))
  | Bop_and, Val_tensor t1, Val_tensor t2 -> Ok (Val_tensor Tensor.(logical_and t1 t2))
  | Bop_or, Val_tensor t1, Val_tensor t2 -> Ok (Val_tensor Tensor.(logical_or t1 t2))
  | _ -> bad_impl "eval_bop"

let rec interp_exp env exp =
  match exp.exp_desc with
  | E_var ident ->
    begin
      match Option.value_exn (lookup_env env ident.txt) with
      | Fval_base value -> Ok value
      | Fval_poly gen_value -> Ok (Option.value_exn (gen_value []))
    end

  | E_inst (ident, dims) ->
    let func = Value_ops.fval_to_poly_exn (Option.value_exn (lookup_env env ident.txt)) in
    Ok (Option.value_exn (func dims))

  | E_triv ->
    Ok Val_triv

  | E_bool b ->
    Ok (Val_tensor (Tensor.mk_b b))

  | E_real r ->
    Ok (Val_tensor (Tensor.mk_f r))

  | E_int n ->
    Ok (Val_tensor (Tensor.mk_i n))

  | E_cond (exp0, exp1, exp2) ->
    let%bind value0 = interp_exp env exp0 in
    begin
      match value0 with
      | Val_tensor t0 ->
        if Tensor.bool_value t0 then
          interp_exp env exp1
        else
          interp_exp env exp2
      | _ -> bad_impl "interp_exp E_cond"
    end

  | E_binop (bop, exp1, exp2) ->
    let%bind value1 = interp_exp env exp1 in
    let%bind value2 = interp_exp env exp2 in
    eval_bop bop value1 value2

  | E_abs (name, _, exp0) ->
    Ok (Val_abs (name.txt, exp0, snd env))

  | E_app (exp1, exp2) ->
    let%bind value1 = interp_exp env exp1 in
    let%bind value2 = interp_exp env exp2 in
    begin
      match value1 with
      | Val_prim_func prim_func -> prim_func value2
      | Val_abs (name, exp0, closure) ->
        interp_exp (fst env, Map.set closure ~key:name ~data:value2) exp0
      | _ -> bad_impl "interp_exp E_app"
    end

  | E_let (exp1, name, exp2) ->
    let%bind value1 = interp_exp env exp1 in
    interp_exp (update_env env ~key:name.txt ~data:value1) exp2

  | E_stack mexps ->
    let mexp = Multi_internal mexps in
    let has_real = ref false in
    let has_bool = ref false in
    let rec collect_values = function
      | Multi_leaf exp0 ->
        let%bind value0 = interp_exp env exp0 in
        let () =
          match value0 with
          | Val_tensor t0 ->
            begin
              match Tensor.kind t0 with
              | Torch_core.Kind.(T Half)
              | Torch_core.Kind.(T Float)
              | Torch_core.Kind.(T Double) -> has_real := true
              | Torch_core.Kind.(T Bool) -> has_bool := true
              | _ -> ()
            end
          | _ -> ()
        in
        Ok (Multi_leaf value0)
      | Multi_internal subs ->
        let%bind subs = Utils.fold_right_result subs
            ~init:[]
            ~f:(fun sub acc ->
                let%bind sub_value = collect_values sub in
                Ok (sub_value :: acc)
              )
        in
        Ok (Multi_internal subs)
    in
    let%bind mvalue = collect_values mexp in
    let rec shape_of = function
      | Multi_leaf _ -> []
      | Multi_internal subs ->
        let n = List.length subs in
        let sub = List.hd_exn subs in
        n :: shape_of sub
    in
    let dims = shape_of mvalue in
    if !has_bool then
      let arr = Bigarray.Genarray.create Bigarray.Int8_unsigned Bigarray.C_layout (Array.of_list dims) in
      let rec assign_elems pos = function
        | Multi_leaf value0 ->
          begin
            match value0 with
            | Val_tensor t0 -> Ok (Bigarray.Genarray.set arr (Array.of_list_rev pos) (Bool.to_int (Tensor.bool_value t0)))
            | _ -> bad_impl "interp_exp E_stack"
          end
        | Multi_internal subs ->
          Or_error.try_with (fun () ->
              List.iteri subs ~f:(fun i sub ->
                  Or_error.ok_exn (assign_elems (i :: pos) sub)
                )
            )
      in
      let%bind () = assign_elems [] mvalue in
      Ok (Val_tensor (Tensor.(to_type (of_bigarray arr) ~type_:Torch_core.Kind.(T Bool))))
    else if !has_real then
      let arr = Bigarray.Genarray.create Bigarray.Float32 Bigarray.C_layout (Array.of_list dims) in
      let rec assign_elems pos = function
        | Multi_leaf value0 ->
          begin
            match value0 with
            | Val_tensor t0 ->
              begin
                match Tensor.kind t0 with
                | Torch_core.Kind.(T Half)
                | Torch_core.Kind.(T Float)
                | Torch_core.Kind.(T Double) -> Ok (Bigarray.Genarray.set arr (Array.of_list_rev pos) (Tensor.float_value t0))
                | Torch_core.Kind.(T Uint8)
                | Torch_core.Kind.(T Int8)
                | Torch_core.Kind.(T Int16)
                | Torch_core.Kind.(T Int)
                | Torch_core.Kind.(T Int64) -> Ok (Bigarray.Genarray.set arr (Array.of_list_rev pos) (Float.of_int (Tensor.int_value t0)))
                | _ -> bad_impl "interp_exp E_stack"
              end
            | _ -> bad_impl "interp_exp E_stack"
          end
        | Multi_internal subs ->
          Or_error.try_with (fun () ->
              List.iteri subs ~f:(fun i sub ->
                  Or_error.ok_exn (assign_elems (i :: pos) sub)
                )
            )
      in
      let%bind () = assign_elems [] mvalue in
      Ok (Val_tensor (Tensor.of_bigarray arr))
    else
      let arr = Bigarray.Genarray.create Bigarray.Int32 Bigarray.C_layout (Array.of_list dims) in
      let rec assign_elems pos = function
        | Multi_leaf value0 ->
          begin
            match value0 with
            | Val_tensor t0 -> Ok (Bigarray.Genarray.set arr (Array.of_list_rev pos) (Int32.of_int_exn (Tensor.int_value t0)))
            | _ -> bad_impl "interp_exp E_stack"
          end
        | Multi_internal subs ->
          Or_error.try_with (fun () ->
              List.iteri subs ~f:(fun i sub ->
                  Or_error.ok_exn (assign_elems (i :: pos) sub)
                )
            )
      in
      let%bind () = assign_elems [] mvalue in
      Ok (Val_tensor (Tensor.of_bigarray arr))

  | E_index (base_exp, index_exps) ->
    let%bind base_value = interp_exp env base_exp in
    let%bind indexes = Utils.fold_right_result
        index_exps
        ~init:[]
        ~f:(fun index_exp acc ->
            let%bind index_value = interp_exp env index_exp in
            match index_value with
            | Val_tensor index_t -> Ok (Tensor.int_value index_t :: acc)
            | _ -> bad_impl "interp_exp E_index"
          )
    in
    begin
      match base_value with
      | Val_tensor base_t ->
        Ok (Val_tensor (List.fold_left indexes ~init:base_t ~f:(fun acc index -> Tensor.get acc index)))
      | _ -> bad_impl "interp_exp E_index"
    end

  | E_tuple exps ->
    let%bind values =
      Utils.fold_right_result exps
        ~init:[]
        ~f:(fun exp0 acc ->
            let%bind value0 = interp_exp env exp0 in
            Ok (value0 :: acc)
          )
    in
    Ok (Val_tuple values)

  | E_field (exp0, field) ->
    let%bind value0 = interp_exp env exp0 in
    begin
      match value0 with
      | Val_tuple values -> Ok (List.nth_exn values field)
      | _ -> bad_impl "interp_exp E_field"
    end

let interp_system proc_defs func_defs { sys_buffer; sys_model; sys_guide; sys_input_channel; sys_output_channel } =
  let func_env = ref String.Map.empty in
  func_env :=
    String.Map.map func_defs
      ~f:(fun (args, body) ->
          let names, _ = List.unzip args in
          Val_prim_func (
            function
            | Val_triv -> interp_exp (stdlib_env, !func_env) body
            | Val_tuple values -> interp_exp (stdlib_env, List.fold2_exn names values ~init:!func_env ~f:(fun acc name value -> Map.set acc ~key:name ~data:value)) body
            | value -> interp_exp (stdlib_env, Map.set !func_env ~key:(List.hd_exn names) ~data:value) body
          )
        );
  let qu_for_output = Queue.create () in
  let enqueue ch ev =
    let qu = Map.find_exn sys_buffer ch in
    let () = Queue.enqueue qu ev in
    if String.(ch = sys_output_channel) then
      Queue.enqueue qu_for_output ev
  in
  let is_halt subr =
    let (cmd, cont) = subr.subr_cont in
    match cmd, cont with
    | (Either.Second _, Cont_stop) -> true
    | _ -> false
  in
  let step subr =
    match subr.subr_cont with
    | Either.Second _, Cont_stop -> Ok false
    | Either.Second value, Cont_bind (name_opt, cmd, env0, cont0) ->
      begin
        if Option.is_some name_opt then
          subr.subr_env <- Map.set env0 ~key:(Option.value_exn name_opt) ~data:value;
        subr.subr_cont <- (Either.first cmd, cont0);
        Ok true
      end
    | Either.Second value, Cont_loop (niter, bind_name, cmd, env0, cont0) ->
      if niter <= 0 then
        begin
          subr.subr_env <- env0;
          subr.subr_cont <- (Either.second value, cont0);
          Ok true
        end
      else
        begin
          subr.subr_env <- Map.set env0 ~key:bind_name ~data:value;
          subr.subr_cont <- (Either.first cmd, Cont_loop (niter - 1, bind_name, cmd, env0, cont0));
          Ok true
        end
    | Either.Second value, Cont_iter (tensors, iter_name, bind_name, cmd, env0, cont0) ->
      begin
        match tensors with
        | [] ->
          begin
            subr.subr_env <- env0;
            subr.subr_cont <- (Either.second value, cont0);
            Ok true
          end
        | th :: tt ->
          begin
            subr.subr_env <- Map.set (Map.set env0 ~key:iter_name ~data:(Val_tensor th)) ~key:bind_name ~data:value;
            subr.subr_cont <- (Either.first cmd, Cont_iter (tt, iter_name, bind_name, cmd, env0, cont0));
            Ok true
          end
      end
    | Either.First cmd, cont ->
      begin
        match cmd.cmd_desc with
        | M_ret exp ->
          let%bind value = interp_exp (stdlib_env, subr.subr_env) exp in
          begin
            subr.subr_cont <- (Either.second value, cont);
            Ok true
          end
        | M_bnd (cmd1, name_opt, cmd2) ->
          begin
            subr.subr_cont <- (Either.first cmd1, Cont_bind (Option.map name_opt ~f:(fun name -> name.txt), cmd2, subr.subr_env, cont));
            Ok true
          end
        | M_branch_self (exp0, cmd1, cmd2) ->
          let%bind value0 = interp_exp (stdlib_env, subr.subr_env) exp0 in
          begin
            match value0 with
            | Val_tensor t0 ->
              let next_cmd = if Tensor.bool_value t0 then cmd1 else cmd2 in
              begin
                subr.subr_cont <- (Either.first next_cmd, cont);
                Ok true
              end
            | _ -> bad_impl "step M_branch_self"
          end
        | M_branch_send (exp0, cmd1, cmd2, channel_name) ->
          let%bind value0 = interp_exp (stdlib_env, subr.subr_env) exp0 in
          begin
            match value0 with
            | Val_tensor t0 ->
              let br = Tensor.bool_value t0 in
              let next_cmd = if br then cmd1 else cmd2 in
              begin
                subr.subr_cont <- (Either.first next_cmd, cont);
                enqueue channel_name.txt
                  (if Poly.(Some channel_name.txt = subr.subr_channel_left) then Ev_branch_right br else Ev_branch_left br);
                Ok true
              end
            | _ -> bad_impl "step M_branch_send"
          end
        | M_branch_recv (cmd1, cmd2, channel_name) ->
          let qu = Map.find_exn sys_buffer channel_name.txt in
          begin
            match Queue.peek qu with
            | Some (Ev_branch_left br) when Poly.(Some channel_name.txt = subr.subr_channel_left) ->
              let next_cmd = if br then cmd1 else cmd2 in
              begin
                ignore (Queue.dequeue qu : event option);
                subr.subr_cont <- (Either.first next_cmd, cont);
                Ok true
              end
            | Some (Ev_branch_right br) when Poly.(Some channel_name.txt = subr.subr_channel_right) ->
              let next_cmd = if br then cmd1 else cmd2 in
              begin
                ignore (Queue.dequeue qu : event option);
                subr.subr_cont <- (Either.first next_cmd, cont);
                Ok true
              end
            | _ -> Ok false
          end
        | M_sample_send (exp, channel_name) ->
          let%bind value = interp_exp (stdlib_env, subr.subr_env) exp in
          begin
            match value with
            | Val_dist lit_dist ->
              if String.(channel_name.txt = sys_input_channel) then
                let qu = Map.find_exn sys_buffer channel_name.txt in
                match Queue.dequeue_exn qu with
                | Ev_tensor_left t
                | Ev_tensor_right t ->
                  begin
                    subr.subr_log_prob_sum <- Tensor.(subr.subr_log_prob_sum + lit_dist#log_prob t);
                    subr.subr_cont <- (Either.second (Val_tensor t), cont);
                    Ok true
                  end
                | _ -> bad_impl "step M_sample_send"
              else
                let sample = lit_dist#sample () in
                begin
                  subr.subr_log_prob_sum <- Tensor.(subr.subr_log_prob_sum + lit_dist#log_prob sample);
                  subr.subr_cont <- (Either.second (Val_tensor sample), cont);
                  enqueue channel_name.txt
                    (if Poly.(Some channel_name.txt = subr.subr_channel_left) then Ev_tensor_right sample else Ev_tensor_left sample);
                  Ok true
                end
            | _ -> bad_impl "step M_sample_send"
          end
        | M_sample_recv (exp, channel_name) ->
          let%bind value = interp_exp (stdlib_env, subr.subr_env) exp in
          begin
            match value with
            | Val_dist lit_dist ->
              begin
                let qu = Map.find_exn sys_buffer channel_name.txt in
                match Queue.peek qu with
                | Some (Ev_tensor_left t) when Poly.(Some channel_name.txt = subr.subr_channel_left) ->
                  begin
                    ignore (Queue.dequeue qu : event option);
                    subr.subr_log_prob_sum <- Tensor.(subr.subr_log_prob_sum + lit_dist#log_prob t);
                    subr.subr_cont <- (Either.second (Val_tensor t), cont);
                    Ok true
                  end
                | Some (Ev_tensor_right t) when Poly.(Some channel_name.txt = subr.subr_channel_right) ->
                  begin
                    ignore (Queue.dequeue qu : event option);
                    subr.subr_log_prob_sum <- Tensor.(subr.subr_log_prob_sum + lit_dist#log_prob t);
                    subr.subr_cont <- (Either.second (Val_tensor t), cont);
                    Ok true
                  end
                | _ -> Ok false
              end
            | _ -> bad_impl "step M_sample_recv"
          end
        | M_call (ident, exps) ->
          let%bind values =
            Utils.fold_right_result exps ~init:[] ~f:(fun exp acc ->
                let%bind value = interp_exp (stdlib_env, subr.subr_env) exp in
                Ok (value :: acc)
              )
          in
          let (proc_sig, proc_body) = Map.find_exn proc_defs ident.txt in
          let theta_env =
            List.fold_left proc_sig.psigv_theta_tys ~init:(!func_env)
              ~f:(fun acc (name, _) -> Map.set acc ~key:name ~data:(Map.find_exn subr.subr_env name))
          in
          let init_env =
            List.fold2_exn proc_sig.psigv_param_tys values ~init:theta_env
              ~f:(fun acc (param, _) value -> Map.set acc ~key:param ~data:value)
          in
          begin
            subr.subr_env <- init_env;
            subr.subr_cont <- (Either.first proc_body, cont);
            Ok true
          end
        | M_loop (niter, init_exp, bind_name, _, cmd) ->
          let%bind init_value = interp_exp (stdlib_env, subr.subr_env) init_exp in
          begin
            subr.subr_cont <- (Either.second init_value, Cont_loop (niter, bind_name.txt, cmd, subr.subr_env, cont));
            Ok true
          end
        | M_iter (iter_exp, init_exp, iter_name, bind_name, _, cmd) ->
          let%bind iter_value = interp_exp (stdlib_env, subr.subr_env) iter_exp in
          let%bind init_value = interp_exp (stdlib_env, subr.subr_env) init_exp in
          begin
            match iter_value with
            | Val_tensor iter_t ->
              let ts = Tensor.to_list iter_t in
              begin
                subr.subr_cont <- (Either.second init_value, Cont_iter (ts, iter_name.txt, bind_name.txt, cmd, subr.subr_env, cont));
                Ok true
              end
            | _ -> bad_impl "step M_iter"
          end
      end
  in
  let rec eval subr =
    let%bind proceed = step subr in
    if proceed then eval subr
    else Ok ()
  in
  let rec loop () =
    if is_halt sys_model && is_halt sys_guide then
      Ok ()
    else
      let%bind () = eval sys_model in
      let%bind () = eval sys_guide in
      loop ()
  in
  let%bind () = loop () in
  let events = Queue.to_list qu_for_output in
  Ok (events, sys_model.subr_log_prob_sum, sys_guide.subr_log_prob_sum)

let infer_system algo proc_defs func_defs system_spec =
  (* TODO: Here I assume there is only one trace. In the future, I can implement batched inference. *)
  let trace = List.hd_exn system_spec.sys_spec_input_traces in
  let system, model_th, guide_th = Trace_ops.create_system system_spec trace in
  let func_env = ref String.Map.empty in
  func_env :=
    String.Map.map func_defs
      ~f:(fun (args, body) ->
          let names, _ = List.unzip args in
          Val_prim_func (
            function
            | Val_triv -> interp_exp (stdlib_env, !func_env) body
            | Val_tuple values -> interp_exp (stdlib_env, List.fold2_exn names values ~init:!func_env ~f:(fun acc name value -> Map.set acc ~key:name ~data:value)) body
            | value -> interp_exp (stdlib_env, Map.set !func_env ~key:(List.hd_exn names) ~data:value) body
          )
        );
  let generate_value tyv exp_opt =
    match exp_opt with
    | Some exp -> interp_exp (stdlib_env, !func_env) exp
    | None ->
      match tyv with
      | Btyv_tensor (pty, dims) ->
        begin
          match pty with
          | Pty_real ->
            Ok (Val_tensor (Tensor.randn dims))
          | Pty_preal
          | Pty_ureal ->
            Ok (Val_tensor (Tensor.rand dims))
          | _ -> Or_error.of_exn (Eval_error ("invalid theta initialization", Location.none))
        end
      | _ -> Or_error.of_exn (Eval_error ("invalid theta initialization", Location.none))
  in
  let%bind () = Or_error.try_with (fun () ->
      system.sys_model.subr_env <- List.fold_left model_th ~init:system.sys_model.subr_env
          ~f:(fun acc (name, tyv, exp_opt) ->
              Map.set acc ~key:name ~data:(generate_value tyv exp_opt |> Or_error.ok_exn)
            );
      system.sys_guide.subr_env <- List.fold_left guide_th ~init:system.sys_guide.subr_env
          ~f:(fun acc (name, tyv, exp_opt) ->
              Map.set acc ~key:name ~data:(generate_value tyv exp_opt |> Or_error.ok_exn)
            )
    )
  in
  match algo with
  | Algo_importance { imp_nsamples } ->
    let%bind samples =
      Tqdm.Tqdm.with_bar imp_nsamples ~f:(fun tqdm ->
          Or_error.try_with (fun () ->
              List.init imp_nsamples ~f:(fun i ->
                  let () = Tqdm.Tqdm.update tqdm (imp_nsamples - i) in
                  let (tr, lm, lg) = interp_system proc_defs func_defs system
                                     |> Or_error.ok_exn
                  in
                  (tr, Tensor.(lm - lg)))))
    in
    let m = Py.Import.add_module "ocaml" in
    Py.Module.set m "samples" (Py.List.of_list_map (fun (tr, lp) ->
        Py.Tuple.of_pair (Trace_ops.py_trace tr, Trace_ops.py_tensor lp)
      ) samples);
    Py.Module.set m "filename" (Py.String.of_string system_spec.sys_spec_output_filename);
    ignore (Py.Run.eval ~start:Py.File "
from ocaml import samples, filename
import pickle
f = open(filename, 'wb')
pickle.dump(samples, f)
f.close()" : Pytypes.pyobject);
    Ok ()

  | Algo_svi _ ->
    failwith "todo"
