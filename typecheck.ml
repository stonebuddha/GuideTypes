open Core
open Ast_types
open Infer_types
open Trace_types
open Or_error.Let_syntax

exception Type_error of string * Location.t

let prelude_ctxt = String.Map.of_alist_exn [
    "T", Libtensor.prelude;
    "D", Libdist.prelude;
  ]

let join_prim ~loc pty1 pty2 =
  let pty1, pty2 =
    if compare_prim_ty pty1 pty2 > 0 then
      pty2, pty1
    else
      pty1, pty2
  in
  match pty1, pty2 with
  | Pty_bool, Pty_bool -> Ok Pty_bool
  | Pty_ureal, Pty_ureal -> Ok Pty_ureal
  | Pty_ureal, Pty_preal -> Ok Pty_preal
  | Pty_ureal, Pty_real -> Ok Pty_real
  | Pty_ureal, Pty_fnat n -> if n <= 2 then Ok Pty_ureal else Ok Pty_preal
  | Pty_ureal, Pty_nat -> Ok Pty_preal
  | Pty_ureal, Pty_int -> Ok Pty_real
  | Pty_preal, Pty_preal -> Ok Pty_preal
  | Pty_preal, Pty_real -> Ok Pty_real
  | Pty_preal, Pty_fnat _ -> Ok Pty_preal
  | Pty_preal, Pty_nat -> Ok Pty_preal
  | Pty_preal, Pty_int -> Ok Pty_real
  | Pty_real, Pty_real -> Ok Pty_real
  | Pty_real, Pty_fnat _ -> Ok Pty_real
  | Pty_real, Pty_nat -> Ok Pty_real
  | Pty_real, Pty_int -> Ok Pty_real
  | Pty_fnat n1, Pty_fnat n2 -> Ok (Pty_fnat (max n1 n2))
  | Pty_fnat _, Pty_nat -> Ok Pty_nat
  | Pty_fnat _, Pty_int -> Ok Pty_int
  | Pty_nat, Pty_nat -> Ok Pty_nat
  | Pty_nat, Pty_int -> Ok Pty_int
  | Pty_int, Pty_int -> Ok Pty_int
  | _ -> Or_error.of_exn (Type_error ("join error", loc))

let meet_prim ~loc pty1 pty2 =
  let pty1, pty2 =
    if compare_prim_ty pty1 pty2 > 0 then
      pty2, pty1
    else
      pty1, pty2
  in
  match pty1, pty2 with
  | Pty_bool, Pty_bool -> Ok Pty_bool
  | Pty_ureal, Pty_ureal -> Ok Pty_ureal
  | Pty_ureal, Pty_preal -> Ok Pty_ureal
  | Pty_ureal, Pty_real -> Ok Pty_ureal
  | Pty_ureal, Pty_fnat n -> if n <= 2 then Ok (Pty_fnat n) else Ok (Pty_fnat 2)
  | Pty_ureal, Pty_nat -> Ok (Pty_fnat 2)
  | Pty_ureal, Pty_int -> Ok (Pty_fnat 2)
  | Pty_preal, Pty_preal -> Ok Pty_preal
  | Pty_preal, Pty_real -> Ok Pty_preal
  | Pty_preal, Pty_fnat n -> Ok (Pty_fnat n)
  | Pty_preal, Pty_nat -> Ok Pty_nat
  | Pty_preal, Pty_int -> Ok Pty_nat
  | Pty_real, Pty_real -> Ok Pty_real
  | Pty_real, Pty_fnat n -> Ok (Pty_fnat n)
  | Pty_real, Pty_nat -> Ok Pty_nat
  | Pty_real, Pty_int -> Ok Pty_int
  | Pty_fnat n1, Pty_fnat n2 -> Ok (Pty_fnat (min n1 n2))
  | Pty_fnat n, Pty_nat -> Ok (Pty_fnat n)
  | Pty_fnat n, Pty_int -> Ok (Pty_fnat n)
  | Pty_nat, Pty_nat -> Ok Pty_nat
  | Pty_nat, Pty_int -> Ok Pty_nat
  | Pty_int, Pty_int -> Ok Pty_int
  | _ -> Or_error.of_exn (Type_error ("meet error", loc))

let is_prim_subtype pty1 pty2 =
  match join_prim ~loc:Location.none pty1 pty2 with
  | Ok pty -> equal_prim_ty pty pty2
  | Error _ -> false

let is_prim_numeric pty = is_prim_subtype pty Pty_real

let is_prim_integer pty = is_prim_subtype pty Pty_int

let cast_prim_to_real = function
  | Pty_bool -> None
  | Pty_ureal -> Some Pty_ureal
  | Pty_preal -> Some Pty_preal
  | Pty_real -> Some Pty_real
  | Pty_fnat n -> if n <= 2 then Some Pty_ureal else Some Pty_preal
  | Pty_nat -> Some Pty_preal
  | Pty_int -> Some Pty_real

let equal_shape = List.equal Int.equal

let rec is_subtype tyv1 tyv2 =
  match tyv1, tyv2 with
  | Btyv_unit, Btyv_unit -> true
  | Btyv_dist tyv1', Btyv_dist tyv2' -> equal_base_tyv tyv1' tyv2'
  | Btyv_arrow (tyv11, tyv12), Btyv_arrow (tyv21, tyv22) -> is_subtype tyv21 tyv11 && is_subtype tyv12 tyv22
  | Btyv_tensor (pty1, dims1), Btyv_tensor (pty2, dims2) -> equal_shape dims1 dims2 && is_prim_subtype pty1 pty2
  | Btyv_simplex n1, Btyv_simplex n2 -> n1 = n2
  | Btyv_simplex n1, Btyv_tensor (pty2, dims2) -> equal_shape [n1] dims2 && is_prim_subtype Pty_ureal pty2
  | Btyv_var ident1, Btyv_var ident2 -> equal_long_ident ident1 ident2
  | Btyv_product tyvs1, Btyv_product tyvs2 ->
    List.length tyvs1 = List.length tyvs2
    && List.for_all2_exn tyvs1 tyvs2 ~f:is_subtype
  | _ -> false

let rec join_type ~loc tyv1 tyv2 =
  match tyv1, tyv2 with
  | Btyv_unit, Btyv_unit ->
    Ok Btyv_unit
  | Btyv_dist tyv1', Btyv_dist tyv2' when equal_base_tyv tyv1' tyv2' ->
    Ok (Btyv_dist tyv1')
  | Btyv_arrow (tyv11, tyv12), Btyv_arrow (tyv21, tyv22) ->
    let%bind tyv1' = meet_type ~loc tyv11 tyv21 in
    let%bind tyv2' = join_type ~loc tyv12 tyv22 in
    Ok (Btyv_arrow (tyv1', tyv2'))
  | Btyv_tensor (pty1, dims1), Btyv_tensor (pty2, dims2) when equal_shape dims1 dims2 ->
    let%bind pty = join_prim ~loc pty1 pty2 in
    Ok (Btyv_tensor (pty, dims1))
  | Btyv_simplex n1, Btyv_simplex n2 when n1 = n2 ->
    Ok (Btyv_simplex n1)
  | Btyv_simplex n1, Btyv_tensor (pty2, dims2) when equal_shape [n1] dims2 ->
    let%bind pty = join_prim ~loc Pty_ureal pty2 in
    Ok (Btyv_tensor (pty, [n1]))
  | Btyv_tensor (pty1, dims1), Btyv_simplex n2 when equal_shape dims1 [n2] ->
    let%bind pty = join_prim ~loc pty1 Pty_ureal in
    Ok (Btyv_tensor (pty, dims1))
  | Btyv_var ident1, Btyv_var ident2 when equal_long_ident ident1 ident2 ->
    Ok (Btyv_var ident1)
  | Btyv_product tyvs1, Btyv_product tyvs2 when List.length tyvs1 = List.length tyvs2 ->
    let%bind tyvs = Utils.fold_right_result (List.zip_exn tyvs1 tyvs2)
        ~init:[]
        ~f:(fun (tyv1, tyv2) acc ->
            let%bind tyv = join_type ~loc tyv1 tyv2 in
            Ok (tyv :: acc)
          )
    in
    Ok (Btyv_product tyvs)
  | _ ->
    Or_error.of_exn (Type_error ("join error", loc))

and meet_type ~loc tyv1 tyv2 =
  match tyv1, tyv2 with
  | Btyv_unit, Btyv_unit ->
    Ok Btyv_unit
  | Btyv_dist tyv1', Btyv_dist tyv2' when equal_base_tyv tyv1' tyv2' ->
    Ok (Btyv_dist tyv1')
  | Btyv_arrow (tyv11, tyv12), Btyv_arrow (tyv21, tyv22) ->
    let%bind tyv1' = join_type ~loc tyv11 tyv21 in
    let%bind tyv2' = meet_type ~loc tyv12 tyv22 in
    Ok (Btyv_arrow (tyv1', tyv2'))
  | Btyv_tensor (pty1, dims1), Btyv_tensor (pty2, dims2) when equal_shape dims1 dims2 ->
    let%bind pty = meet_prim ~loc pty1 pty2 in
    Ok (Btyv_tensor (pty, dims1))
  | Btyv_simplex n1, Btyv_simplex n2 when n1 = n2 ->
    Ok (Btyv_simplex n1)
  | Btyv_simplex n1, Btyv_tensor (pty2, dims2) when equal_shape [n1] dims2 ->
    let%bind pty = meet_prim ~loc Pty_ureal pty2 in
    Ok (Btyv_tensor (pty, [n1]))
  | Btyv_tensor (pty1, dims1), Btyv_simplex n2 when equal_shape dims1 [n2] ->
    let%bind pty = meet_prim ~loc pty1 Pty_ureal in
    Ok (Btyv_tensor (pty, dims1))
  | Btyv_var ident1, Btyv_var ident2 when equal_long_ident ident1 ident2 ->
    Ok (Btyv_var ident1)
  | Btyv_product tyvs1, Btyv_product tyvs2 when List.length tyvs1 = List.length tyvs2 ->
    let%bind tyvs = Utils.fold_right_result (List.zip_exn tyvs1 tyvs2)
        ~init:[]
        ~f:(fun (tyv1, tyv2) acc ->
            let%bind tyv = meet_type ~loc tyv1 tyv2 in
            Ok (tyv :: acc)
          )
    in
    Ok (Btyv_product tyvs)
  | _ ->
    Or_error.of_exn (Type_error ("meet error", loc))

let rec eval_ty ty =
  match ty.bty_desc with
  | Bty_unit -> Btyv_unit
  | Bty_arrow (ty1, ty2) -> Btyv_arrow (eval_ty ty1, eval_ty ty2)
  | Bty_dist ty0 -> Btyv_dist (eval_ty ty0)
  | Bty_tensor (pty, dims) -> Btyv_tensor (pty, dims)
  | Bty_simplex n -> Btyv_simplex n
  | Bty_var ident -> Btyv_var ident.txt
  | Bty_product tys -> Btyv_product (List.map tys ~f:eval_ty)

let tycheck_bop_prim bop pty1 pty2 =
  match bop.txt with
  | Bop_sub ->
    begin
      if is_prim_integer pty1 && is_prim_integer pty2 then
        Ok Pty_int
      else if is_prim_numeric pty1 && is_prim_numeric pty2 then
        Ok Pty_real
      else
        Or_error.of_exn (Type_error ("mismatched operand types", bop.loc))
    end
  | Bop_div ->
    begin
      match cast_prim_to_real pty1, cast_prim_to_real pty2 with
      | Some pty1', Some pty2' ->
        begin
          match pty1', pty2' with
          | Pty_real, _
          | _, Pty_real -> Ok Pty_real
          | _ -> Ok Pty_preal
        end
      | _ ->
        Or_error.of_exn (Type_error ("mismatched operand types", bop.loc))
    end
  | _ ->
    let pty1, pty2 =
      if compare_prim_ty pty1 pty2 > 0 then
        pty2, pty1
      else
        pty1, pty2
    in
    begin
      match bop.txt, pty1, pty2 with
      | Bop_add, Pty_ureal, Pty_ureal -> Ok Pty_preal
      | Bop_add, Pty_ureal, Pty_preal -> Ok Pty_preal
      | Bop_add, Pty_ureal, Pty_real -> Ok Pty_real
      | Bop_add, Pty_ureal, Pty_fnat _ -> Ok Pty_preal
      | Bop_add, Pty_ureal, Pty_nat -> Ok Pty_preal
      | Bop_add, Pty_ureal, Pty_int -> Ok Pty_real
      | Bop_add, Pty_preal, Pty_preal -> Ok Pty_preal
      | Bop_add, Pty_preal, Pty_real -> Ok Pty_real
      | Bop_add, Pty_preal, Pty_fnat _ -> Ok Pty_preal
      | Bop_add, Pty_preal, Pty_nat -> Ok Pty_preal
      | Bop_add, Pty_preal, Pty_int -> Ok Pty_real
      | Bop_add, Pty_real, Pty_real -> Ok Pty_real
      | Bop_add, Pty_real, Pty_fnat _ -> Ok Pty_real
      | Bop_add, Pty_real, Pty_nat -> Ok Pty_real
      | Bop_add, Pty_real, Pty_int -> Ok Pty_real
      | Bop_add, Pty_fnat n, Pty_fnat m -> Ok (Pty_fnat (n + m))
      | Bop_add, Pty_fnat _, Pty_nat -> Ok Pty_nat
      | Bop_add, Pty_fnat _, Pty_int -> Ok Pty_int
      | Bop_add, Pty_nat, Pty_nat -> Ok Pty_nat
      | Bop_add, Pty_nat, Pty_int -> Ok Pty_int
      | Bop_add, Pty_int, Pty_int -> Ok Pty_int

      | Bop_mul, Pty_ureal, Pty_ureal -> Ok Pty_ureal
      | Bop_mul, Pty_ureal, Pty_preal -> Ok Pty_preal
      | Bop_mul, Pty_ureal, Pty_real -> Ok Pty_real
      | Bop_mul, Pty_ureal, Pty_fnat n -> if n <= 2 then Ok Pty_ureal else Ok Pty_preal
      | Bop_mul, Pty_ureal, Pty_nat -> Ok Pty_preal
      | Bop_mul, Pty_ureal, Pty_int -> Ok Pty_real
      | Bop_mul, Pty_preal, Pty_preal -> Ok Pty_preal
      | Bop_mul, Pty_preal, Pty_real -> Ok Pty_real
      | Bop_mul, Pty_preal, Pty_fnat _ -> Ok Pty_preal
      | Bop_mul, Pty_preal, Pty_nat -> Ok Pty_preal
      | Bop_mul, Pty_preal, Pty_int -> Ok Pty_real
      | Bop_mul, Pty_real, Pty_real -> Ok Pty_real
      | Bop_mul, Pty_real, Pty_fnat _ -> Ok Pty_real
      | Bop_mul, Pty_real, Pty_nat -> Ok Pty_real
      | Bop_mul, Pty_real, Pty_int -> Ok Pty_real
      | Bop_mul, Pty_fnat n, Pty_fnat m -> Ok (Pty_fnat (n * m))
      | Bop_mul, Pty_fnat _, Pty_nat -> Ok Pty_nat
      | Bop_mul, Pty_fnat _, Pty_int -> Ok Pty_int
      | Bop_mul, Pty_nat, Pty_nat -> Ok Pty_nat
      | Bop_mul, Pty_nat, Pty_int -> Ok Pty_int
      | Bop_mul, Pty_int, Pty_int -> Ok Pty_int

      | Bop_eq, pty1, pty2
      | Bop_ne, pty1, pty2 when Bool.(is_prim_numeric pty1 = is_prim_numeric pty2) -> Ok Pty_bool

      | Bop_lt, pty1, pty2
      | Bop_le, pty1, pty2
      | Bop_gt, pty1, pty2
      | Bop_ge, pty1, pty2 when is_prim_numeric pty1 && is_prim_numeric pty2 -> Ok Pty_bool

      | Bop_and, Pty_bool, Pty_bool
      | Bop_or, Pty_bool, Pty_bool -> Ok Pty_bool

      | _ -> Or_error.of_exn (Type_error ("mismatched operand types", bop.loc))
    end

let tycheck_bop bop arg1 arg2 =
  match arg1, arg2 with
  | Btyv_tensor (pty1, dims1), Btyv_tensor (pty2, dims2) when equal_shape dims1 dims2 ->
    let%bind pty = tycheck_bop_prim bop pty1 pty2 in
    Ok (Btyv_tensor (pty, dims1))
  | Btyv_simplex n1, Btyv_simplex n2 when n1 = n2 ->
    let%bind pty = tycheck_bop_prim bop Pty_ureal Pty_ureal in
    Ok (Btyv_tensor (pty, [n1]))
  | Btyv_simplex n1, Btyv_tensor (pty2, dims2) when equal_shape [n1] dims2 ->
    let%bind pty = tycheck_bop_prim bop Pty_ureal pty2 in
    Ok (Btyv_tensor (pty, [n1]))
  | Btyv_tensor (pty1, dims1), Btyv_simplex n2 when equal_shape dims1 [n2] ->
    let%bind pty = tycheck_bop_prim bop pty1 Pty_ureal in
    Ok (Btyv_tensor (pty, dims1))
  | _ ->
    Or_error.of_exn (Type_error ("mismatched operand types", bop.loc))

let lookup_ctx (libs, cur) lid =
  match lid with
  | Lident_name name ->
    Option.map (Map.find cur name) ~f:(fun tyv -> Ftyv_base tyv)
  | Lident_path (lib_name, name) ->
    begin
      match Map.find libs lib_name with
      | None -> None
      | Some lib -> Map.find lib name
    end

let update_ctx (libs, cur) ~key ~data =
  (libs, Map.set cur ~key ~data)

let rec tycheck_multilayer ~loc chk mexp =
  match mexp with
  | Multi_leaf exp ->
    let%bind tyv = chk exp in
    begin
      match tyv with
      | Btyv_tensor (pty, []) -> Ok ([], pty)
      | _ -> Or_error.of_exn (Type_error ("non-primitive element type", loc))
    end
  | Multi_internal subs ->
    let%bind res =
      List.fold_result subs ~init:None ~f:(fun acc sub ->
          let%bind (sub_dims, sub_pty) = tycheck_multilayer ~loc chk sub in
          match acc with
          | None -> Ok (Some (sub_dims, sub_pty))
          | Some (dims, pty) ->
            if equal_shape dims sub_dims then
              let%bind join_pty = join_prim ~loc pty sub_pty in
              Ok (Some (sub_dims, join_pty))
            else
              Or_error.of_exn (Type_error ("illegal tensor shape", loc))
        )
    in
    let (dims, pty) = Option.value_exn res in
    Ok (List.length subs :: dims, pty)

let rec tycheck_exp ctxt exp =
  match exp.exp_desc with
  | E_inst (ident, dims) ->
    begin
      match lookup_ctx ctxt ident.txt with
      | Some (Ftyv_poly gen_tyv) ->
        begin
          match (gen_tyv dims) with
          | Some tyv -> Ok tyv
          | None -> Or_error.of_exn (Type_error ("invalid instantiation of " ^ Ast_ops.string_of_long_ident ident.txt, exp.exp_loc))
        end
      | _ -> Or_error.of_exn (Type_error ("undefined variable " ^ Ast_ops.string_of_long_ident ident.txt, exp.exp_loc))
    end

  | E_var ident ->
    begin
      match lookup_ctx ctxt ident.txt with
      | Some (Ftyv_base tyv) -> Ok tyv
      | Some (Ftyv_poly gen_tyv) ->
        begin
          match gen_tyv [] with
          | Some tyv -> Ok tyv
          | None -> Or_error.of_exn (Type_error ("invalid instantiation of " ^ Ast_ops.string_of_long_ident ident.txt, exp.exp_loc))
        end
      | _ -> Or_error.of_exn (Type_error ("undefined variable " ^ Ast_ops.string_of_long_ident ident.txt, exp.exp_loc))
    end

  | E_triv -> Ok Btyv_unit

  | E_bool _ -> Ok (Btyv_tensor (Pty_bool, []))

  | E_cond (exp0, exp1, exp2) ->
    let%bind tyv0 = tycheck_exp ctxt exp0 in
    if is_subtype tyv0 (Btyv_tensor (Pty_bool, [])) then
      let%bind tyv1 = tycheck_exp ctxt exp1 in
      let%bind tyv2 = tycheck_exp ctxt exp2 in
      join_type ~loc:exp.exp_loc tyv1 tyv2
    else
      Or_error.of_exn (Type_error ("non-boolean condition type", exp0.exp_loc))

  | E_real r ->
    if Float.(r >= 0. && r <= 1.) then
      Ok (Btyv_tensor (Pty_ureal, []))
    else if Float.(r >= 0.) then
      Ok (Btyv_tensor (Pty_preal, []))
    else
      Ok (Btyv_tensor (Pty_real, []))

  | E_int n ->
    if n >= 0 then
      Ok (Btyv_tensor (Pty_fnat (n + 1), []))
    else
      Ok (Btyv_tensor (Pty_int, []))

  | E_binop (bop, exp1, exp2) ->
    let%bind tyv1 = tycheck_exp ctxt exp1 in
    let%bind tyv2 = tycheck_exp ctxt exp2 in
    tycheck_bop bop tyv1 tyv2

  | E_abs (name, ty, exp0) ->
    let tyv = eval_ty ty in
    let%bind tyv0 = tycheck_exp (update_ctx ctxt ~key:name.txt ~data:tyv) exp0 in
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
          Or_error.of_exn (Type_error ("mismatched argument types", exp2.exp_loc))
      | _ ->
        Or_error.of_exn (Type_error ("non-arrow function type", exp1.exp_loc))
    end

  | E_let (exp1, name, exp2) ->
    let%bind tyv1 = tycheck_exp ctxt exp1 in
    let ctxt' = update_ctx ctxt ~key:name.txt ~data:tyv1 in
    tycheck_exp ctxt' exp2

  | E_stack mexps ->
    let mexp = Multi_internal mexps in
    let%bind (dims, pty) = tycheck_multilayer ~loc:exp.exp_loc (tycheck_exp ctxt) mexp in
    if is_prim_numeric pty && List.length dims = 1 && (
        let prob = List.fold_result mexps
            ~init:0.0
            ~f:(fun acc mexp ->
                match mexp with
                | Multi_leaf { exp_desc = E_real r; _ } when Float.(r >= 0.) -> Ok Float.(acc + r)
                | Multi_leaf { exp_desc = E_int n; _ } when n >= 0 -> Ok Float.(acc + of_int n)
                | _ -> Error ()
              )
        in
        match prob with
        | Error () -> false
        | Ok prob -> Float.(abs (prob - 1.) < Utils.float_eps)
      ) then
      Ok (Btyv_simplex (List.hd_exn dims))
    else
      Ok (Btyv_tensor (pty, dims))

  | E_index (base_exp, index_exps) ->
    let%bind base_tyv = tycheck_exp ctxt base_exp in
    begin
      let%bind (pty, dims) =
        match base_tyv with
        | Btyv_tensor (pty, dims) -> Ok (pty, dims)
        | Btyv_simplex n -> Ok (Pty_ureal, [n])
        | _ -> Or_error.of_exn (Type_error ("not indexable", base_exp.exp_loc))
      in
      if List.length dims < List.length index_exps then
        Or_error.of_exn (Type_error ("mismatched dimension", exp.exp_loc))
      else
        let mat_dims, res_dims = List.split_n dims (List.length index_exps) in
        let%bind () = List.fold_result (List.zip_exn mat_dims index_exps) ~init:() ~f:(fun () (dim, index_exp) ->
            let%bind index_tyv = tycheck_exp ctxt index_exp in
            match index_tyv with
            | Btyv_tensor (pty, []) when is_prim_subtype pty (Pty_fnat dim) ->
              Ok ()
            | _ ->
              Or_error.of_exn (Type_error ("invalid index", index_exp.exp_loc))
          )
        in
        Ok (Btyv_tensor (pty, res_dims))
    end

  | E_tuple exps ->
    let%bind tyvs = Utils.fold_right_result exps
        ~init:[]
        ~f:(fun exp0 acc ->
            let%bind tyv0 = tycheck_exp ctxt exp0 in
            Ok (tyv0 :: acc)
          )
    in
    Ok (Btyv_product tyvs)

  | E_field (exp0, field) ->
    let%bind tyv0 = tycheck_exp ctxt exp0 in
    begin
      match tyv0 with
      | Btyv_product tyvs ->
        if field >= 0 && field < List.length tyvs then
          Ok (List.nth_exn tyvs field)
        else
          Or_error.of_exn (Type_error ("invalid field", exp.exp_loc))
      | _ ->
        Or_error.of_exn (Type_error ("non-projectable value", exp0.exp_loc))
    end

let rec eval_sty sty =
  match sty.sty_desc with
  | Sty_one -> Styv_one
  | Sty_conj (ty1, sty2) -> Styv_conj (eval_ty ty1, eval_sty sty2)
  | Sty_imply (ty1, sty2) -> Styv_imply (eval_ty ty1, eval_sty sty2)
  | Sty_ichoice (sty1, sty2) -> Styv_ichoice (eval_sty sty1, eval_sty sty2)
  | Sty_echoice (sty1, sty2) -> Styv_echoice (eval_sty sty1, eval_sty sty2)
  | Sty_var (ident, None) -> Styv_var (ident.txt, Styv_one)
  | Sty_var (ident, Some sty0) -> Styv_var (ident.txt, eval_sty sty0)

let collect_sess_tys prog =
  List.fold_result prog ~init:(Hashtbl.create (module String)) ~f:(fun acc top ->
      match top with
      | Top_proc _ -> Ok acc
      | Top_sess (name, sty) ->
        begin
          match Hashtbl.add acc ~key:name.txt ~data:(Option.map ~f:eval_sty sty) with
          | `Ok -> Ok acc
          | `Duplicate -> Or_error.of_exn (Type_error ("duplicate toplevel type identifier", name.loc))
        end
      | Top_func _ -> Ok acc
    )

let eval_proc_sig psig =
  let psigv =
    { psigv_theta_tys = List.map psig.psig_theta_tys ~f:(fun (name, pty) -> (name.txt, eval_ty pty))
    ; psigv_param_tys = List.map psig.psig_param_tys ~f:(fun (name, ty) -> (name.txt, eval_ty ty))
    ; psigv_ret_ty = eval_ty psig.psig_ret_ty
    ; psigv_sess_left = Option.map psig.psig_sess_left ~f:(fun (channel_name, type_name) -> (channel_name.txt, type_name.txt))
    ; psigv_sess_right = Option.map psig.psig_sess_right ~f:(fun (channel_name, type_name) -> (channel_name.txt, type_name.txt))
    }
  in
  match psigv.psigv_sess_left, psigv.psigv_sess_right with
  | Some (ch_left, _), Some (ch_right, _) when String.(ch_left = ch_right) ->
    Or_error.of_exn (Type_error ("left and right channels coincide", (Option.value_exn psig.psig_sess_right |> fst).loc))
  | _->
    Ok psigv

let collect_proc_sigs prog =
  List.fold_result prog ~init:String.Map.empty ~f:(fun acc top ->
      match top with
      | Top_sess _ -> Ok acc
      | Top_proc (name, { proc_sig; _ }) ->
        let%bind proc_sigv = eval_proc_sig proc_sig in
        begin
          match Map.add acc ~key:name.txt ~data:proc_sigv with
          | `Ok acc' -> Ok acc'
          | `Duplicate -> Or_error.of_exn (Type_error ("duplicate toplevel procedure identifier", name.loc))
        end
      | Top_func _ -> Ok acc
    )

let collect_func_sigs prog =
  List.fold_result prog ~init:String.Map.empty ~f:(fun acc top ->
      match top with
      | Top_sess _ -> Ok acc
      | Top_proc _ -> Ok acc
      | Top_func (name, { func_param_tys; func_ret_ty; _ }) ->
        match Map.add acc ~key:name.txt ~data:(Btyv_arrow (
            (match func_param_tys with
             | [] -> Btyv_unit
             | [(_, ty)] -> eval_ty ty
             | _ -> Btyv_product (List.map func_param_tys ~f:(fun (_, ty) -> eval_ty ty))),
            eval_ty func_ret_ty
          )) with
        | `Ok acc' -> Ok acc'
        | `Duplicate -> Or_error.of_exn (Type_error ("duplicate toplevel function identifier", name.loc))
    )

let collect_proc_defs_exn prog =
  String.Map.of_alist_exn (List.filter_map prog ~f:(fun top ->
      match top with
      | Top_sess _ -> None
      | Top_proc (name, { proc_sig; proc_body; _ }) -> Some (name.txt, (Or_error.ok_exn (eval_proc_sig proc_sig), proc_body))
      | Top_func _ -> None
    ))

let collect_func_defs_exn prog =
  String.Map.of_alist_exn (List.filter_map prog ~f:(fun top ->
      match top with
      | Top_sess _ -> None
      | Top_proc _ -> None
      | Top_func (name, { func_param_tys; func_body; _ }) -> Some (name.txt, (List.map func_param_tys ~f:(fun (param, ty) -> (param.txt, eval_ty ty)), func_body))
    ))

let tycheck_cmd psig_ctxt =
  let rec forward ctxt cmd =
    match cmd.cmd_desc with
    | M_ret exp ->
      tycheck_exp ctxt exp

    | M_bnd (cmd1, name_opt, cmd2) ->
      let%bind tyv1 = forward ctxt cmd1 in
      let ctxt' = match name_opt with
        | None -> ctxt
        | Some name ->
          update_ctx ctxt ~key:name.txt ~data:tyv1
      in
      forward ctxt' cmd2

    | M_call (name, exps) ->
      begin
        match Map.find psig_ctxt name.txt with
        | None -> Or_error.of_exn (Type_error ("unknown procedure " ^ name.txt, name.loc))
        | Some psigv ->
          if List.length psigv.psigv_param_tys <> List.length exps then
            Or_error.of_exn (Type_error ("mismatched arity", cmd.cmd_loc))
          else
            let%bind tyvs = Utils.fold_right_result exps ~init:[] ~f:(fun exp acc ->
                let%bind tyv = tycheck_exp ctxt exp in
                Ok (tyv :: acc)
              )
            in
            if not (List.for_all2_exn tyvs psigv.psigv_param_tys ~f:(fun tyv (_, tyv') -> is_subtype tyv tyv')) then
              Or_error.of_exn (Type_error ("mismatched argument types", cmd.cmd_loc))
            else
              Ok psigv.psigv_ret_ty
      end

    | M_sample_recv (exp, _)
    | M_sample_send (exp, _) ->
      let%bind tyv = tycheck_exp ctxt exp in
      begin
        match tyv with
        | Btyv_dist tyv0 -> Ok tyv0
        | _ -> Or_error.of_exn (Type_error ("non-distribution types", exp.exp_loc))
      end

    | M_branch_recv (cmd1, cmd2, _) ->
      let%bind tyv1 = forward ctxt cmd1 in
      let%bind tyv2 = forward ctxt cmd2 in
      join_type ~loc:cmd.cmd_loc tyv1 tyv2

    | M_branch_send (exp, cmd1, cmd2, _)
    | M_branch_self (exp, cmd1, cmd2) ->
      let%bind tyv = tycheck_exp ctxt exp in
      if is_subtype tyv (Btyv_tensor (Pty_bool, [])) then
        let%bind tyv1 = forward ctxt cmd1 in
        let%bind tyv2 = forward ctxt cmd2 in
        join_type ~loc:cmd.cmd_loc tyv1 tyv2
      else
        Or_error.of_exn (Type_error ("non-boolean condition type", exp.exp_loc))

    | M_loop (_, init_exp, bind_name, bind_ty, cmd0) ->
      let%bind tyv = tycheck_exp ctxt init_exp in
      let bind_tyv = eval_ty bind_ty in
      if is_subtype tyv bind_tyv then
        let ctxt' = update_ctx ctxt ~key:bind_name.txt ~data:bind_tyv in
        let%bind tyv' = forward ctxt' cmd0 in
        if is_subtype tyv' bind_tyv then
          Ok bind_tyv
        else
          Or_error.of_exn (Type_error ("inconsistent result type in loop", cmd0.cmd_loc))
      else
        Or_error.of_exn (Type_error ("inconsistent intial value for loop", init_exp.exp_loc))

    | M_iter (iter_exp, init_exp, iter_name, bind_name, bind_ty, cmd0) ->
      let%bind iter_tyv = tycheck_exp ctxt iter_exp in
      begin
        match iter_tyv with
        | Btyv_tensor (pty, dims) when List.length dims > 0 ->
          let elem_tyv = Btyv_tensor (pty, List.tl_exn dims) in
          let%bind init_tyv = tycheck_exp ctxt init_exp in
          let bind_tyv = eval_ty bind_ty in
          if is_subtype init_tyv bind_tyv then
            let ctxt' = update_ctx ctxt ~key:iter_name.txt ~data:elem_tyv in
            let ctxt'' = update_ctx ctxt' ~key:bind_name.txt ~data:bind_tyv in
            let%bind tyv' = forward ctxt'' cmd0 in
            if is_subtype tyv' bind_tyv then
              Ok bind_tyv
            else
              Or_error.of_exn (Type_error ("inconsistent result type in iter", cmd0.cmd_loc))
          else
            Or_error.of_exn (Type_error ("inconsistent initial value for iter", init_exp.exp_loc))
        | _ ->
          Or_error.of_exn (Type_error ("not iterable", iter_exp.exp_loc))
      end
  in

  let rec backward ctxt sess cmd =
    match cmd.cmd_desc with
    | M_ret _ ->
      Ok sess

    | M_bnd (cmd1, name_opt, cmd2) ->
      let%bind tyv1 = forward ctxt cmd1 in
      let ctxt' =
        match name_opt with
        | None -> ctxt
        | Some name ->
          update_ctx ctxt ~key:name.txt ~data:tyv1
      in
      let%bind sess' = backward ctxt' sess cmd2 in
      backward ctxt sess' cmd1

    | M_sample_recv (_, channel_name) ->
      let%bind tyv = forward ctxt cmd in
      begin
        match Map.find sess channel_name.txt with
        | None -> Or_error.of_exn (Type_error ("unknown channel " ^ channel_name.txt, channel_name.loc))
        | Some (`Left, sty) ->
          Ok (Map.set sess ~key:channel_name.txt ~data:(`Left, Styv_conj (tyv, sty)))
        | Some (`Right, sty) ->
          Ok (Map.set sess ~key:channel_name.txt ~data:(`Right, Styv_imply (tyv, sty)))
      end

    | M_sample_send (_, channel_name) ->
      let%bind tyv = forward ctxt cmd in
      begin
        match Map.find sess channel_name.txt with
        | None -> Or_error.of_exn (Type_error ("unknown channel " ^ channel_name.txt, channel_name.loc))
        | Some (`Left, sty) ->
          Ok (Map.set sess ~key:channel_name.txt ~data:(`Left, Styv_imply (tyv, sty)))
        | Some (`Right, sty) ->
          Ok (Map.set sess ~key:channel_name.txt ~data:(`Right, Styv_conj (tyv, sty)))
      end

    | M_branch_recv (cmd1, cmd2, channel_name) ->
      let%bind sess1 = backward ctxt sess cmd1 in
      let%bind sess2 = backward ctxt sess cmd2 in
      Or_error.try_with (fun () ->
          Map.merge sess1 sess2 ~f:(fun ~key -> function
              | `Left _
              | `Right _ -> assert false
              | `Both ((dir1, styv1), (_, styv2)) ->
                if String.(key = channel_name.txt) then
                  match dir1 with
                  | `Left -> Some (`Left, Styv_ichoice (styv1, styv2))
                  | `Right -> Some (`Right, Styv_echoice (styv1, styv2))
                else if equal_sess_tyv styv1 styv2 then
                  Some (dir1, styv1)
                else
                  raise (Type_error ("mismatched sessions", cmd.cmd_loc))
            )
        )

    | M_branch_send (_, cmd1, cmd2, channel_name) ->
      let%bind sess1 = backward ctxt sess cmd1 in
      let%bind sess2 = backward ctxt sess cmd2 in
      Or_error.try_with (fun () ->
          Map.merge sess1 sess2 ~f:(fun ~key -> function
              | `Left _
              | `Right _ -> assert false
              | `Both ((dir1, styv1), (_, styv2)) ->
                if String.(key = channel_name.txt) then
                  match dir1 with
                  | `Left -> Some (`Left, Styv_echoice (styv1, styv2))
                  | `Right -> Some (`Right, Styv_ichoice (styv1, styv2))
                else if equal_sess_tyv styv1 styv2 then
                  Some (dir1, styv1)
                else
                  raise (Type_error ("mismatched sessions", cmd.cmd_loc))
            )
        )

    | M_branch_self (_, cmd1, cmd2) ->
      let%bind sess1 = backward ctxt sess cmd1 in
      let%bind sess2 = backward ctxt sess cmd2 in
      Or_error.try_with (fun () ->
          Map.merge sess1 sess2 ~f:(fun ~key:_ -> function
              | `Left _
              | `Right _ -> assert false
              | `Both ((dir1, styv1), (_, styv2)) ->
                if equal_sess_tyv styv1 styv2 then
                  Some (dir1, styv1)
                else
                  raise (Type_error ("mismatched sessions", cmd.cmd_loc))
            )
        )

    | M_call (name, _) ->
      begin
        match Map.find psig_ctxt name.txt with
        | None -> Or_error.of_exn (Type_error ("unknown procedure " ^ name.txt, name.loc))
        | Some psigv ->
          let sess0 = String.Map.of_alist_exn
              (List.append (Option.to_list psigv.psigv_sess_left) (Option.to_list psigv.psigv_sess_right)) in
          if not (Set.is_subset (Map.key_set sess0) ~of_:(Map.key_set sess)) then
            Or_error.of_exn (Type_error ("mismatched channels", cmd.cmd_loc))
          else
            Or_error.try_with (fun () ->
                Map.merge sess0 sess ~f:(fun ~key:_ -> function
                    | `Left _ -> assert false
                    | `Right (dir, sty) -> Some (dir, sty)
                    | `Both (type_id, (dir, sty)) ->
                      Some (dir, Styv_var (type_id, sty))
                  )
              )
      end

    | M_loop (n, _, bind_name, bind_ty, cmd0) ->
      let bind_tyv = eval_ty bind_ty in
      let ctxt' = update_ctx ctxt ~key:bind_name.txt ~data:bind_tyv in
      List.fold_result (List.init n ~f:(fun _ -> ())) ~init:sess
        ~f:(fun acc () -> backward ctxt' acc cmd0)

    | M_iter (iter_exp, _, iter_name, bind_name, bind_ty, cmd0) ->
      let%bind iter_tyv = tycheck_exp ctxt iter_exp in
      begin
        match iter_tyv with
        | Btyv_tensor (pty, dims) when List.length dims > 0 ->
          let elem_tyv = Btyv_tensor (pty, List.tl_exn dims) in
          let bind_tyv = eval_ty bind_ty in
          let ctxt' = update_ctx ctxt ~key:iter_name.txt ~data:elem_tyv in
          let ctxt'' = update_ctx ctxt' ~key:bind_name.txt ~data:bind_tyv in
          List.fold_result (List.init (List.hd_exn dims) ~f:(fun _ -> ())) ~init:sess
            ~f:(fun acc () -> backward ctxt'' acc cmd0)
        | _ ->
          Or_error.of_exn (Type_error ("not iterable", iter_exp.exp_loc))
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
        Option.map sess_right ~f:(fun (channel_id, _) -> let (_, sty) = Map.find_exn sess' channel_id in (channel_id, sty)))

let rec subst_sty styv0 = function
  | Styv_one -> styv0
  | Styv_conj (tyv, styv) -> Styv_conj (tyv, subst_sty styv0 styv)
  | Styv_imply (tyv, styv) -> Styv_imply (tyv, subst_sty styv0 styv)
  | Styv_ichoice (styv1, styv2) -> Styv_ichoice (subst_sty styv0 styv1, subst_sty styv0 styv2)
  | Styv_echoice (styv1, styv2) -> Styv_echoice (subst_sty styv0 styv1, subst_sty styv0 styv2)
  | Styv_var (type_id, styv_inst) -> Styv_var (type_id, subst_sty styv0 styv_inst)

let rec tycheck_trace ~loc sty_ctxt tr styv =
  match styv with
  | Styv_var (type_id, styv0) ->
    let styv_def = Hashtbl.find_exn sty_ctxt type_id |> Option.value_exn in
    tycheck_trace ~loc sty_ctxt tr (subst_sty styv0 styv_def)
  | Styv_one ->
    if List.is_empty tr then
      Ok ()
    else
      Or_error.of_exn (Type_error ("invalid trace", loc))
  | Styv_conj (_, styv0) ->
    begin
      match tr with
      | Ev_tensor_left _ :: tr' -> tycheck_trace ~loc sty_ctxt tr' styv0
      | _ -> Or_error.of_exn (Type_error ("invalid trace", loc))
    end
  | Styv_imply (_, styv0) ->
    begin
      match tr with
      | Ev_tensor_right _ :: tr' -> tycheck_trace ~loc sty_ctxt tr' styv0
      | _ -> Or_error.of_exn (Type_error ("invalid trace", loc))
    end
  | Styv_ichoice (styv1, styv2) ->
    begin
      match tr with
      | Ev_branch_left b :: tr' -> tycheck_trace ~loc sty_ctxt tr' (if b then styv1 else styv2)
      | _ -> Or_error.of_exn (Type_error ("invalid trace", loc))
    end
  | Styv_echoice (styv1, styv2) ->
    begin
      match tr with
      | Ev_branch_right b :: tr' -> tycheck_trace ~loc sty_ctxt tr' (if b then styv1 else styv2)
      | _ -> Or_error.of_exn (Type_error ("invalid trace", loc))
    end

let tycheck_func func_ctxt func =
  let%bind ctxt = List.fold_result func.func_param_tys ~init:func_ctxt ~f:(fun acc (name, ty) ->
      match Map.add acc ~key:name.txt ~data:(eval_ty ty) with
      | `Ok acc' -> Ok acc'
      | `Duplicate -> Or_error.of_exn (Type_error ("duplicate identifier", name.loc))
    )
  in
  let%bind ret_tyv = tycheck_exp (prelude_ctxt, ctxt) func.func_body in
  let decl_tyv = eval_ty func.func_ret_ty in
  if is_subtype ret_tyv decl_tyv then
    Ok ()
  else
    Or_error.of_exn (Type_error ("incorrect function return type", func.func_loc))

let tycheck_proc sty_ctxt psig_ctxt func_ctxt proc =
  let%bind psigv = eval_proc_sig proc.proc_sig in
  let%bind ctxt = List.fold_result (List.append (List.map psigv.psigv_theta_tys ~f:(fun (name, tyv) -> (name, tyv))) psigv.psigv_param_tys)
      ~init:func_ctxt ~f:(fun acc (name, tyv) ->
          match Map.add acc ~key:name ~data:tyv with
          | `Ok acc' -> Ok acc'
          | `Duplicate -> Or_error.of_exn (Type_error ("duplicate identifier", proc.proc_loc))
        )
  in
  let%bind (tyv, sess_left, sess_right) =
    tycheck_cmd psig_ctxt (prelude_ctxt, ctxt)
      (Option.map psigv.psigv_sess_left ~f:(fun (channel_id, _) -> (channel_id, Styv_one)))
      (Option.map psigv.psigv_sess_right ~f:(fun (channel_id, _) -> (channel_id, Styv_one)))
      proc.proc_body
  in
  if not (is_subtype tyv psigv.psigv_ret_ty) then
    Or_error.of_exn (Type_error ("mismatched signature types", proc.proc_loc))
  else if Option.value_map sess_left ~default:false ~f:(fun (_, sty) ->
      let type_id = Option.value_exn psigv.psigv_sess_left |> snd in
      match Hashtbl.find sty_ctxt type_id with
      | None -> true
      | Some sty_def ->
        match sty_def with
        | None ->
          Format.printf "inferred session:@.\ttype %s[$] = %a@." type_id Ast_ops.print_sess_tyv sty;
          Hashtbl.set sty_ctxt ~key:type_id ~data:(Some sty);
          false
        | Some sty_def ->
          not (equal_sess_tyv sty sty_def)
    ) then
    Or_error.of_exn (Type_error ("mismatched left session", proc.proc_loc))
  else if Option.value_map sess_right ~default:false ~f:(fun (_, sty) ->
      let type_id = Option.value_exn psigv.psigv_sess_right |> snd in
      match Hashtbl.find sty_ctxt type_id with
      | None -> true
      | Some sty_def ->
        match sty_def with
        | None ->
          Format.printf "inferred session:@.\ttype %s[$] = %a@." type_id Ast_ops.print_sess_tyv sty;
          Hashtbl.set sty_ctxt ~key:type_id ~data:(Some sty);
          false
        | Some sty_def ->
          not (equal_sess_tyv sty sty_def)
    ) then
    Or_error.of_exn (Type_error ("mismatched right session", proc.proc_loc))
  else
    Ok ()

let rec verify_sess_ty sty_ctxt sty =
  match sty.sty_desc with
  | Sty_one -> Ok ()
  | Sty_conj (_, sty2) -> verify_sess_ty sty_ctxt sty2
  | Sty_imply (_, sty2) -> verify_sess_ty sty_ctxt sty2
  | Sty_ichoice (sty1, sty2) ->
    let%bind () = verify_sess_ty sty_ctxt sty1 in
    verify_sess_ty sty_ctxt sty2
  | Sty_echoice (sty1, sty2) ->
    let%bind () = verify_sess_ty sty_ctxt sty1 in
    verify_sess_ty sty_ctxt sty2
  | Sty_var (ident, sty0) ->
    match Hashtbl.find sty_ctxt ident.txt with
    | None -> Or_error.of_exn (Type_error ("unknown type " ^ ident.txt, ident.loc))
    | Some _ -> Option.value_map sty0 ~default:(Ok ()) ~f:(verify_sess_ty sty_ctxt)

let tycheck_script sty_ctxt psig_ctxt func_ctxt script =
  let (model, model_theta, model_args) = script.inf_model in
  let (guide, guide_theta, guide_args) = script.inf_guide in
  let (input_ch, input_file) = script.inf_input in
  let (output_ch, output_file) = script.inf_output in

  let check_mcall ~loc mpsigv margs mtheta =
    if List.length mpsigv.psigv_param_tys <> List.length margs then
      Or_error.of_exn (Type_error ("mismatched arity", loc))
    else if List.length mpsigv.psigv_theta_tys <> List.length mtheta then
      Or_error.of_exn (Type_error ("mismatched theta", loc))
    else
      let%bind () = List.fold_result (List.zip_exn mpsigv.psigv_theta_tys mtheta)
          ~init:()
          ~f:(fun () ((_, tyv), exp_opt) ->
              match exp_opt with
              | Some exp ->
                let%bind chk_tyv = tycheck_exp (prelude_ctxt, func_ctxt) exp in
                if is_subtype chk_tyv tyv then
                  Ok ()
                else
                  Or_error.of_exn (Type_error ("mismatched theta types", exp.exp_loc))
              | None ->
                Ok ()
            )
      in
      List.fold_result (List.zip_exn mpsigv.psigv_param_tys margs)
        ~init:()
        ~f:(fun () ((_, tyv), exp) ->
            let%bind chk_tyv = tycheck_exp (prelude_ctxt, func_ctxt) exp in
            if is_subtype chk_tyv tyv then
              Ok ()
            else
              Or_error.of_exn (Type_error ("mismatched argument types", exp.exp_loc))
          )
  in
  let%bind psigv_model =
    match Map.find psig_ctxt model.txt with
    | Some psigv_model -> Ok psigv_model
    | None -> Or_error.of_exn (Type_error ("unknown procedure identifier", model.loc))
  in
  let%bind () = check_mcall ~loc:model.loc psigv_model model_args model_theta in
  let%bind psigv_guide =
    match Map.find psig_ctxt guide.txt with
    | Some psigv_guide -> Ok psigv_guide
    | None -> Or_error.of_exn (Type_error ("unknown procedure identifier", guide.loc))
  in
  let%bind () = check_mcall ~loc:guide.loc psigv_guide guide_args guide_theta in

  let%bind lat_ch, lat_sty =
    if Option.is_some psigv_guide.psigv_sess_left then
      Or_error.of_exn (Type_error ("guide should not consume any channel", guide.loc))
    else
      match psigv_guide.psigv_sess_right with
      | None -> Or_error.of_exn (Type_error ("guide should provide a channel", guide.loc))
      | Some channel_spec -> Ok channel_spec
  in

  let%bind obs_ch, obs_sty =
    match psigv_model.psigv_sess_left with
    | None -> Or_error.of_exn (Type_error ("model should consume a channel", model.loc))
    | Some (channel_name, channel_sty) ->
      if String.(lat_ch <> channel_name || lat_sty <> channel_sty) then
        Or_error.of_exn (Type_error ("mismatched model-guide pair", model.loc))
      else
        match psigv_model.psigv_sess_right with
        | None -> Or_error.of_exn (Type_error ("model should provide a channel", model.loc))
        | Some channel_spec -> Ok channel_spec
  in

  let%bind () =
    if String.(obs_ch <> input_ch.txt) then
      Or_error.of_exn (Type_error ("mismatched input channel", input_ch.loc))
    else
      Ok ()
  in
  let%bind () =
    if String.(lat_ch <> output_ch.txt) then
      Or_error.of_exn (Type_error ("mismatched output channel", output_ch.loc))
    else
      Ok ()
  in

  let%bind traces =
    match Sys.file_exists input_file.txt with
    | `No | `Unknown -> Or_error.of_exn (Type_error ("input file not found", input_file.loc))
    | `Yes ->
      let parse_channel ch =
        let lexbuf = Lexing.from_channel ch in
        Location.init lexbuf input_file.txt;
        Location.input_name := input_file.txt;
        Location.input_lexbuf := Some lexbuf;
        Parse.batch_traces lexbuf
      in
      In_channel.with_file input_file.txt ~f:parse_channel
  in

  let%bind () =
    List.fold_result traces ~init:() ~f:(fun () trace ->
        tycheck_trace ~loc:trace.loc sty_ctxt trace.txt (Styv_var (obs_sty, Styv_one))
      )
  in

  let system_spec =
    { sys_spec_model = { cmd_desc = M_call (model, model_args); cmd_loc = model.loc },
                       List.map2_exn psigv_model.psigv_theta_tys model_theta ~f:(fun (name, tyv) exp_opt -> (name, tyv, exp_opt)),
                       Some lat_ch,
                       Some obs_ch
    ; sys_spec_guide = { cmd_desc = M_call (guide, guide_args); cmd_loc = guide.loc },
                       List.map2_exn psigv_guide.psigv_theta_tys guide_theta ~f:(fun (name, tyv) exp_opt -> (name, tyv, exp_opt)),
                       None,
                       Some lat_ch
    ; sys_spec_input_channel = obs_ch
    ; sys_spec_input_traces = List.map traces ~f:(fun trace -> trace.txt)
    ; sys_spec_output_channel = lat_ch
    ; sys_spec_output_filename = output_file.txt
    }
  in

  Ok (Some (system_spec, script.inf_algo))

let tycheck_prog (prog, script_opt) =
  let%bind sty_ctxt = collect_sess_tys prog in
  let%bind psig_ctxt = collect_proc_sigs prog in
  let%bind func_ctxt = collect_func_sigs prog in
  let%bind () = List.fold_result prog ~init:() ~f:(fun () top ->
      match top with
      | Top_sess (_, sty) ->
        begin
          match sty with
          | None -> Ok ()
          | Some sty ->
            match sty.sty_desc with
            | Sty_var _ -> Or_error.of_exn (Type_error ("non-contractive type", sty.sty_loc))
            | _ -> verify_sess_ty sty_ctxt sty
        end
      | Top_proc (_, proc) -> tycheck_proc sty_ctxt psig_ctxt func_ctxt proc
      | Top_func (_, func) -> tycheck_func func_ctxt func
    )
  in
  let%bind system_spec =
    match script_opt with
    | Some script ->
      tycheck_script sty_ctxt psig_ctxt func_ctxt script
    | None ->
      Ok None
  in
  let proc_defs = collect_proc_defs_exn prog in
  let func_defs = collect_func_defs_exn prog in
  Ok (proc_defs, func_defs, system_spec)

let () =
  Location.register_error_of_exn
    (function
      | Type_error (msg, loc) -> Some (Location.errorf ~loc "%s" msg)
      | _ -> None
    )
