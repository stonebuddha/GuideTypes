open Core
open Ast_types
open Value_types
open Or_error.Let_syntax

exception Eval_error of string * Location.t

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

let stdlib_env = String.Map.of_alist_exn [
    "T", Libtensor.stdlib;
  ]

let eval_bop bop value1 value2 =
  match bop.txt, value1, value2 with
  | Bop_add, Val_int n1, Val_int n2 -> Ok (Val_int (n1 + n2))
  | Bop_add, Val_int n1, Val_real f2 -> Ok (Val_real Float.(of_int n1 + f2))
  | Bop_add, Val_real f1, Val_int n2 -> Ok (Val_real Float.(f1 + of_int n2))
  | Bop_add, Val_real f1, Val_real f2 -> Ok (Val_real Float.(f1 + f2))
  | Bop_add, Val_tensor t1, Val_tensor t2 -> Ok (Val_tensor Tensor.(t1 + t2))

  | Bop_sub, Val_int n1, Val_int n2 -> Ok (Val_int (n1 - n2))
  | Bop_sub, Val_int n1, Val_real f2 -> Ok (Val_real Float.(of_int n1 - f2))
  | Bop_sub, Val_real f1, Val_int n2 -> Ok (Val_real Float.(f1 - of_int n2))
  | Bop_sub, Val_real f1, Val_real f2 -> Ok (Val_real Float.(f1 - f2))
  | Bop_sub, Val_tensor t1, Val_tensor t2 -> Ok (Val_tensor Tensor.(t1 - t2))

  | Bop_mul, Val_int n1, Val_int n2 -> Ok (Val_int (n1 * n2))
  | Bop_mul, Val_int n1, Val_real f2 -> Ok (Val_real Float.(of_int n1 * f2))
  | Bop_mul, Val_real f1, Val_int n2 -> Ok (Val_real Float.(f1 * of_int n2))
  | Bop_mul, Val_real f1, Val_real f2 -> Ok (Val_real Float.(f1 * f2))
  | Bop_mul, Val_tensor t1, Val_tensor t2 -> Ok (Val_tensor Tensor.(t1 * t2))

  | Bop_div, Val_int n1, Val_int n2 -> Ok (Val_real Float.(of_int n1 / of_int n2))
  | Bop_div, Val_int n1, Val_real f2 -> Ok (Val_real Float.(of_int n1 / f2))
  | Bop_div, Val_real f1, Val_int n2 -> Ok (Val_real Float.(f1 / of_int n2))
  | Bop_div, Val_real f1, Val_real f2 -> Ok (Val_real Float.(f1 / f2))
  | Bop_div, Val_tensor t1, Val_tensor t2 -> Ok (Val_tensor Tensor.(t1 / t2))

  | Bop_eq, Val_bool b1, Val_bool b2 -> Ok (Val_bool Bool.(b1 = b2))
  | Bop_eq, Val_int n1, Val_int n2 -> Ok (Val_bool (n1 = n2))
  | Bop_eq, Val_int n1, Val_real f2 -> Ok (Val_bool Float.(of_int n1 = f2))
  | Bop_eq, Val_real f1, Val_int n2 -> Ok (Val_bool Float.(f1 = of_int n2))
  | Bop_eq, Val_real f1, Val_real f2 -> Ok (Val_bool Float.(f1 = f2))
  | Bop_eq, Val_tensor t1, Val_tensor t2 -> Ok (Val_tensor Tensor.(t1 = t2))

  | Bop_ne, Val_bool b1, Val_bool b2 -> Ok (Val_bool Bool.(b1 <> b2))
  | Bop_ne, Val_int n1, Val_int n2 -> Ok (Val_bool (n1 <> n2))
  | Bop_ne, Val_int n1, Val_real f2 -> Ok (Val_bool Float.(of_int n1 <> f2))
  | Bop_ne, Val_real f1, Val_int n2 -> Ok (Val_bool Float.(f1 <> of_int n2))
  | Bop_ne, Val_real f1, Val_real f2 -> Ok (Val_bool Float.(f1 <> f2))
  | Bop_ne, Val_tensor t1, Val_tensor t2 -> Ok (Val_tensor Tensor.(t1 <> t2))

  | Bop_lt, Val_int n1, Val_int n2 -> Ok (Val_bool (n1 < n2))
  | Bop_lt, Val_int n1, Val_real f2 -> Ok (Val_bool Float.(of_int n1 < f2))
  | Bop_lt, Val_real f1, Val_int n2 -> Ok (Val_bool Float.(f1 < of_int n2))
  | Bop_lt, Val_real f1, Val_real f2 -> Ok (Val_bool Float.(f1 < f2))
  | Bop_lt, Val_tensor t1, Val_tensor t2 -> Ok (Val_tensor Tensor.(t1 < t2))

  | Bop_le, Val_int n1, Val_int n2 -> Ok (Val_bool (n1 <= n2))
  | Bop_le, Val_int n1, Val_real f2 -> Ok (Val_bool Float.(of_int n1 <= f2))
  | Bop_le, Val_real f1, Val_int n2 -> Ok (Val_bool Float.(f1 <= of_int n2))
  | Bop_le, Val_real f1, Val_real f2 -> Ok (Val_bool Float.(f1 <= f2))
  | Bop_le, Val_tensor t1, Val_tensor t2 -> Ok (Val_tensor Tensor.(t1 <= t2))

  | Bop_gt, Val_int n1, Val_int n2 -> Ok (Val_bool (n1 > n2))
  | Bop_gt, Val_int n1, Val_real f2 -> Ok (Val_bool Float.(of_int n1 > f2))
  | Bop_gt, Val_real f1, Val_int n2 -> Ok (Val_bool Float.(f1 > of_int n2))
  | Bop_gt, Val_real f1, Val_real f2 -> Ok (Val_bool Float.(f1 > f2))
  | Bop_gt, Val_tensor t1, Val_tensor t2 -> Ok (Val_tensor Tensor.(t1 > t2))

  | Bop_ge, Val_int n1, Val_int n2 -> Ok (Val_bool (n1 >= n2))
  | Bop_ge, Val_int n1, Val_real f2 -> Ok (Val_bool Float.(of_int n1 >= f2))
  | Bop_ge, Val_real f1, Val_int n2 -> Ok (Val_bool Float.(f1 >= of_int n2))
  | Bop_ge, Val_real f1, Val_real f2 -> Ok (Val_bool Float.(f1 >= f2))
  | Bop_ge, Val_tensor t1, Val_tensor t2 -> Ok (Val_tensor Tensor.(t1 >= t2))

  | Bop_and, Val_bool b1, Val_bool b2 -> Ok (Val_bool (b1 && b2))

  | Bop_or, Val_bool b1, Val_bool b2 -> Ok (Val_bool (b1 || b2))

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
    Ok (Val_bool b)

  | E_real r ->
    Ok (Val_real r)

  | E_int n ->
    Ok (Val_int n)

  | E_cond (exp0, exp1, exp2) ->
    let%bind value0 = interp_exp env exp0 in
    begin
      match value0 with
      | Val_bool true -> interp_exp env exp1
      | Val_bool false -> interp_exp env exp2
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

  | E_tensor exp0 ->
    let%bind value0 = interp_exp env exp0 in
    begin
      match value0 with
      | Val_real r -> Ok (Val_tensor (Tensor.mk_f r))
      | Val_int n -> Ok (Val_tensor (Tensor.mk_i n))
      | Val_bool b -> Ok (Val_tensor (Tensor.mk_b b))
      | _ -> bad_impl "interp_exp E_tensor"
    end

  | E_stack mexps ->
    let mexp = Multi_internal mexps in
    let has_real = ref false in
    let rec collect_values = function
      | Multi_leaf exp0 ->
        let%bind value0 = interp_exp env exp0 in
        let () =
          match value0 with
          | Val_real _ -> has_real := true
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
            | Val_real r -> Ok (Bigarray.Genarray.set arr (Array.of_list_rev pos) r)
            | Val_int n -> Ok (Bigarray.Genarray.set arr (Array.of_list_rev pos) (Float.of_int n))
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
            | Val_int n -> Ok (Bigarray.Genarray.set arr (Array.of_list_rev pos) (Int32.of_int_exn n))
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
            | Val_int n -> Ok (n :: acc)
            | _ -> bad_impl "interp_exp E_index"
          )
    in
    begin
      match base_value with
      | Val_tensor tensor ->
        let kind = Tensor.kind tensor in
        begin
          match kind with
          | Torch_core.Kind.(T Half)
          | Torch_core.Kind.(T Float)
          | Torch_core.Kind.(T Double) -> Ok (Val_real (Tensor.float_get tensor indexes))
          | Torch_core.Kind.(T Uint8)
          | Torch_core.Kind.(T Int8)
          | Torch_core.Kind.(T Int16)
          | Torch_core.Kind.(T Int)
          | Torch_core.Kind.(T Int64) -> Ok (Val_int (Tensor.int_get tensor indexes))
          | Torch_core.Kind.(T Bool) -> Ok (Val_bool (Tensor.bool_get tensor indexes))
          | _ -> bad_impl "interp_exp E_index"
        end
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
