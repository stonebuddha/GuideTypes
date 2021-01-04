open Core
open Ast_types
open Value_types
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
    Ok (Value_ops.fval_to_base_exn (Option.value_exn (lookup_env env ident.txt)))

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
    if !has_real then
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
