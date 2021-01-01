open Core
open Ast_types
open Or_error.Let_syntax

exception Type_error of string * Location.t

let is_prim_numeric = function
  | Pty_ureal
  | Pty_preal
  | Pty_real
  | Pty_fnat _
  | Pty_nat
  | Pty_int -> true
  | _ -> false

let is_prim_subtype pty1 pty2 =
  match pty1, pty2 with
  | Pty_unit, Pty_unit -> true
  | Pty_bool, Pty_bool -> true
  | Pty_ureal, Pty_ureal
  | Pty_ureal, Pty_preal
  | Pty_ureal, Pty_real -> true
  | Pty_preal, Pty_preal
  | Pty_preal, Pty_real -> true
  | Pty_real, Pty_real -> true
  | Pty_fnat n, Pty_fnat m -> n <= m
  | Pty_fnat _, Pty_nat
  | Pty_fnat _, Pty_int -> true
  | Pty_fnat n, Pty_ureal -> n <= 2
  | Pty_fnat _, Pty_preal
  | Pty_fnat _, Pty_real -> true
  | Pty_nat, Pty_nat
  | Pty_nat, Pty_int
  | Pty_nat, Pty_preal
  | Pty_nat, Pty_real -> true
  | Pty_int, Pty_int
  | Pty_int, Pty_real -> true
  | _ -> false

let equal_shape = List.equal Int.equal

let rec is_subtype tyv1 tyv2 =
  match tyv1, tyv2 with
  | Btyv_prim pty1, Btyv_prim pty2 -> is_prim_subtype pty1 pty2
  | Btyv_dist tyv1', Btyv_dist tyv2' -> equal_base_tyv tyv1' tyv2'
  | Btyv_arrow (tyv11, tyv12), Btyv_arrow (tyv21, tyv22) -> is_subtype tyv21 tyv11 && is_subtype tyv12 tyv22
  | Btyv_tensor (pty1, dims1), Btyv_tensor (pty2, dims2) when equal_shape dims1 dims2 -> is_prim_subtype pty1 pty2
  | Btyv_simplex n1, Btyv_simplex n2 when n1 = n2 -> true
  | Btyv_simplex n1, Btyv_tensor (pty2, dims2) when equal_shape [n1] dims2 -> is_prim_subtype Pty_ureal pty2
  | Btyv_tensor (pty1, dims1), Btyv_simplex n2 when equal_shape dims1 [n2] -> is_prim_subtype pty1 Pty_ureal
  | Btyv_external name1, Btyv_external name2 -> equal_long_ident name1 name2
  | Btyv_product tyvs1, Btyv_product tyvs2 ->
    List.length tyvs1 = List.length tyvs2
    && List.for_all2_exn tyvs1 tyvs2 ~f:is_subtype
  | _ -> false

let join_prim ~loc pty1 pty2 =
  if is_prim_subtype pty1 pty2 then
    Ok pty2
  else if is_prim_subtype pty2 pty1 then
    Ok pty1
  else
    Or_error.of_exn (Type_error ("join error", loc))

let meet_prim ~loc pty1 pty2 =
  if is_prim_subtype pty1 pty2 then
    Ok pty1
  else if is_prim_subtype pty2 pty1 then
    Ok pty2
  else
    Or_error.of_exn (Type_error ("meet error", loc))

let rec join_type ~loc tyv1 tyv2 =
  match tyv1, tyv2 with
  | Btyv_prim pty1, Btyv_prim pty2 ->
    let%bind pty = join_prim ~loc pty1 pty2 in
    Ok (Btyv_prim pty)
  | Btyv_dist tyv1', Btyv_dist tyv2' ->
    if equal_base_tyv tyv1' tyv2' then
      Ok (Btyv_dist tyv1')
    else
      Or_error.of_exn (Type_error ("join error", loc))
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
  | Btyv_external name1, Btyv_external name2 when equal_long_ident name1 name2 ->
    Ok (Btyv_external name1)
  | Btyv_product tyvs1, Btyv_product tyvs2 ->
    if List.length tyvs1 <> List.length tyvs2 then
      Or_error.of_exn (Type_error ("mismatched tuple sizes", loc))
    else
      let%bind tyvs = List.fold_result (List.zip_exn tyvs1 tyvs2)
          ~init:[]
          ~f:(fun acc (tyv1, tyv2) ->
              let%bind tyv' = join_type ~loc tyv1 tyv2 in
              Ok (tyv' :: acc)
            )
      in
      Ok (Btyv_product (List.rev tyvs))
  | _ ->
    Or_error.of_exn (Type_error ("join error", loc))

and meet_type ~loc tyv1 tyv2 =
  match tyv1, tyv2 with
  | Btyv_prim pty1, Btyv_prim pty2 ->
    let%bind pty = meet_prim ~loc pty1 pty2 in
    Ok (Btyv_prim pty)
  | Btyv_dist tyv1', Btyv_dist tyv2' ->
    if equal_base_tyv tyv1' tyv2' then
      Ok (Btyv_dist tyv1')
    else
      Or_error.of_exn (Type_error ("meet error", loc))
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
  | Btyv_external name1, Btyv_external name2 when equal_long_ident name1 name2 ->
    Ok (Btyv_external name1)
  | Btyv_product tyvs1, Btyv_product tyvs2 ->
    if List.length tyvs1 <> List.length tyvs2 then
      Or_error.of_exn (Type_error ("mismatched tuple sizes", loc))
    else
      let%bind tyvs = List.fold_result (List.zip_exn tyvs1 tyvs2)
          ~init:[]
          ~f:(fun acc (tyv1, tyv2) ->
              let%bind tyv' = meet_type ~loc tyv1 tyv2 in
              Ok (tyv' :: acc)
            )
      in
      Ok (Btyv_product (List.rev tyvs))
  | _ ->
    Or_error.of_exn (Type_error ("meet error", loc))

let rec eval_ty ty =
  match ty.bty_desc with
  | Bty_prim pty -> Btyv_prim pty
  | Bty_arrow (ty1, ty2) -> Btyv_arrow (eval_ty ty1, eval_ty ty2)
  | Bty_dist ty0 -> Btyv_dist (eval_ty ty0)
  | Bty_tensor (pty, dims) -> Btyv_tensor (pty, dims)
  | Bty_simplex n -> Btyv_simplex n
  | Bty_external type_name -> Btyv_external type_name.txt
  | Bty_product tys -> Btyv_product (List.map tys ~f:eval_ty)

let tycheck_bop_prim bop pty1 pty2 =
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
  | Bop_add, Pty_fnat _, Pty_int -> Ok Pty_int
  | Bop_add, Pty_nat, Pty_fnat _
  | Bop_add, Pty_nat, Pty_nat -> Ok Pty_nat
  | Bop_add, Pty_nat, Pty_int -> Ok Pty_int
  | Bop_add, Pty_int, Pty_fnat _
  | Bop_add, Pty_int, Pty_nat
  | Bop_add, Pty_int, Pty_int -> Ok Pty_int

  | Bop_sub, Pty_ureal, Pty_ureal
  | Bop_sub, Pty_ureal, Pty_preal
  | Bop_sub, Pty_ureal, Pty_real -> Ok Pty_real
  | Bop_sub, Pty_preal, Pty_ureal
  | Bop_sub, Pty_preal, Pty_preal
  | Bop_sub, Pty_preal, Pty_real -> Ok Pty_real
  | Bop_sub, Pty_real, Pty_ureal
  | Bop_sub, Pty_real, Pty_preal
  | Bop_sub, Pty_real, Pty_real -> Ok Pty_real
  | Bop_sub, Pty_fnat _, Pty_fnat _
  | Bop_sub, Pty_fnat _, Pty_nat
  | Bop_sub, Pty_fnat _, Pty_int -> Ok Pty_int
  | Bop_sub, Pty_nat, Pty_fnat _
  | Bop_sub, Pty_nat, Pty_nat
  | Bop_sub, Pty_nat, Pty_int -> Ok Pty_int
  | Bop_sub, Pty_int, Pty_fnat _
  | Bop_sub, Pty_int, Pty_nat
  | Bop_sub, Pty_int, Pty_int -> Ok Pty_int

  | Bop_mul, Pty_ureal, Pty_ureal -> Ok Pty_ureal
  | Bop_mul, Pty_ureal, Pty_preal -> Ok Pty_preal
  | Bop_mul, Pty_ureal, Pty_real -> Ok Pty_real
  | Bop_mul, Pty_preal, Pty_ureal
  | Bop_mul, Pty_preal, Pty_preal -> Ok Pty_preal
  | Bop_mul, Pty_preal, Pty_real -> Ok Pty_real
  | Bop_mul, Pty_real, Pty_ureal
  | Bop_mul, Pty_real, Pty_preal
  | Bop_mul, Pty_real, Pty_real -> Ok Pty_real
  | Bop_mul, Pty_fnat n, Pty_fnat m -> Ok (Pty_fnat (n * m))
  | Bop_mul, Pty_fnat _, Pty_nat -> Ok Pty_nat
  | Bop_mul, Pty_fnat _, Pty_int -> Ok Pty_int
  | Bop_mul, Pty_nat, Pty_fnat _
  | Bop_mul, Pty_nat, Pty_nat -> Ok Pty_nat
  | Bop_mul, Pty_nat, Pty_int -> Ok Pty_int
  | Bop_mul, Pty_int, Pty_fnat _
  | Bop_mul, Pty_int, Pty_nat
  | Bop_mul, Pty_int, Pty_int -> Ok Pty_int

  | Bop_div, Pty_ureal, Pty_ureal
  | Bop_div, Pty_ureal, Pty_preal -> Ok Pty_preal
  | Bop_div, Pty_ureal, Pty_real -> Ok Pty_real
  | Bop_div, Pty_preal, Pty_ureal
  | Bop_div, Pty_preal, Pty_preal -> Ok Pty_preal
  | Bop_div, Pty_preal, Pty_real -> Ok Pty_real
  | Bop_div, Pty_real, Pty_ureal
  | Bop_div, Pty_real, Pty_preal
  | Bop_div, Pty_real, Pty_real -> Ok Pty_real

  | Bop_eq, pty1, pty2
  | Bop_ne, pty1, pty2 when is_prim_subtype pty1 pty2 || is_prim_subtype pty2 pty1 -> Ok Pty_bool

  | Bop_lt, pty1, pty2
  | Bop_le, pty1, pty2
  | Bop_gt, pty1, pty2
  | Bop_ge, pty1, pty2 when is_prim_numeric pty1 && (is_prim_subtype pty1 pty2 || is_prim_subtype pty2 pty1) -> Ok Pty_bool

  | Bop_and, Pty_bool, Pty_bool
  | Bop_or, Pty_bool, Pty_bool -> Ok Pty_bool

  | _ -> Or_error.of_exn (Type_error ("mismatched operand types", bop.loc))

let tycheck_bop bop arg1 arg2 =
  match arg1, arg2 with
  | Btyv_prim pty1, Btyv_prim pty2 ->
    let%bind res = tycheck_bop_prim bop pty1 pty2 in
    Ok (Btyv_prim res)
  | Btyv_tensor (pty1, dims1), Btyv_tensor (pty2, dims2) when equal_shape dims1 dims2 ->
    let%bind res = tycheck_bop_prim bop pty1 pty2 in
    Ok (Btyv_tensor (res, dims1))
  | Btyv_simplex n1, Btyv_simplex n2 when n1 = n2 ->
    Ok (Btyv_tensor (Pty_preal, [n1]))
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
    Map.find cur name
  | Lident_path (lib_name, name) ->
    begin
      match Map.find libs lib_name with
      | None -> None
      | Some lib -> Map.find lib name
    end

let update_ctx (libs, cur) ~key ~data =
  (libs, Map.set cur ~key ~data)

let rec tycheck_exp ctxt exp =
  match exp.exp_desc with
  | E_var var_name ->
    begin
      match lookup_ctx ctxt var_name.txt with
      | Some tyv -> Ok tyv
      | None -> Or_error.of_exn (Type_error ("undefined variable " ^ Ast_ops.string_of_long_ident var_name.txt, exp.exp_loc))
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
      Or_error.of_exn (Type_error ("non-boolean condition type", exp0.exp_loc))
  | E_real r ->
    if Float.(r >= 0. && r <= 1.) then
      Ok (Btyv_prim Pty_ureal)
    else if Float.(r >= 0.) then
      Ok (Btyv_prim Pty_preal)
    else
      Ok (Btyv_prim Pty_real)
  | E_nat n ->
    if n >= 0 then
      Ok (Btyv_prim (Pty_fnat (n + 1)))
    else
      Ok (Btyv_prim Pty_int)
  | E_binop (bop, exp1, exp2) ->
    let%bind tyv1 = tycheck_exp ctxt exp1 in
    let%bind tyv2 = tycheck_exp ctxt exp2 in
    tycheck_bop bop tyv1 tyv2
  | E_abs (var_name, ty, exp0) ->
    let tyv = eval_ty ty in
    let%bind tyv0 = tycheck_exp (update_ctx ctxt ~key:var_name.txt ~data:tyv) exp0 in
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
        Or_error.of_exn (Type_error ("non-arrow function type", exp.exp_loc))
    end
  | E_let (exp1, var_name, exp2) ->
    let%bind tyv1 = tycheck_exp ctxt exp1 in
    let ctxt' = update_ctx ctxt ~key:var_name.txt ~data:tyv1 in
    tycheck_exp ctxt' exp2
  | E_dist dist ->
    let%bind tyv = tycheck_dist ~loc:exp.exp_loc ctxt dist in
    Ok (Btyv_dist tyv)
  | E_tensor exp0 ->
    let%bind tyv0 = tycheck_exp ctxt exp0 in
    begin
      match tyv0 with
      | Btyv_prim pty -> Ok (Btyv_tensor (pty, []))
      | _ -> Or_error.of_exn (Type_error ("non-primitive element type", exp0.exp_loc))
    end
  | E_stack exps ->
    let n = List.length exps in
    let%bind ptys = List.fold_result exps ~init:[] ~f:(fun acc exp ->
        let%bind tyv = tycheck_exp ctxt exp in
        match tyv with
        | Btyv_tensor (pty, dims) -> Ok ((pty, dims) :: acc)
        | _ -> Or_error.of_exn (Type_error ("non-tensor type", exp.exp_loc))
      ) in
    let (pty, dims) = List.hd_exn ptys in
    let%bind join_pty = List.fold_result ptys ~init:pty ~f:(fun acc (pty', dims') ->
        if equal_shape dims dims' then
          join_prim ~loc:exp.exp_loc acc pty'
        else
          Or_error.of_exn (Type_error ("not stackable", exp.exp_loc))
      )
    in
    Ok (Btyv_tensor (join_pty, n :: dims))
  | E_index (base_exp, index_exps) ->
    let%bind base_tyv = tycheck_exp ctxt base_exp in
    begin
      match base_tyv with
      | Btyv_tensor (pty, dims) ->
        if List.length dims <> List.length index_exps then
          Or_error.of_exn (Type_error ("mismatched dimension", exp.exp_loc))
        else
          let%bind () = List.fold_result (List.zip_exn dims index_exps) ~init:() ~f:(fun () (dim, index_exp) ->
              let%bind index_tyv = tycheck_exp ctxt index_exp in
              match index_tyv with
              | Btyv_prim pty when is_prim_subtype pty (Pty_fnat dim) ->
                Ok ()
              | _ ->
                Or_error.of_exn (Type_error ("invalid index", index_exp.exp_loc))
            )
          in
          Ok (Btyv_prim pty)
      | _ ->
        Or_error.of_exn (Type_error ("not indexable", base_exp.exp_loc))
    end
  | E_tuple exps ->
    let%bind tyvs = List.fold_result exps
        ~init:[]
        ~f:(fun acc exp ->
            let%bind tyv = tycheck_exp ctxt exp in
            Ok (tyv :: acc)
          )
    in
    Ok (Btyv_product (List.rev tyvs))
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

and tycheck_dist ~loc ctxt dist =
  let lift tars goal curs =
    let tcs = List.zip_exn tars curs in
    let%bind res =
      List.fold_result tcs ~init:None ~f:(fun acc (tar, cur) ->
          match cur with
          | Btyv_prim pty when is_prim_subtype pty tar ->
            begin
              match acc with
              | None -> Ok (Some None)
              | Some None -> Ok (Some None)
              | _ -> Or_error.of_exn (Type_error ("mixed tensors and scalars", loc))
            end
          | Btyv_tensor (pty, dims) when is_prim_subtype pty tar ->
            begin
              match acc with
              | None -> Ok (Some (Some dims))
              | Some None -> Or_error.of_exn (Type_error ("mixed tensors and scalars", loc))
              | Some (Some dims') ->
                if equal_shape dims dims' then
                  Ok (Some (Some dims))
                else
                  Or_error.of_exn (Type_error ("inconsistent tensor dims", loc))
            end
          | _ -> Or_error.of_exn (Type_error ("mismatched parameter types", loc))
        )
    in
    let res = Option.value_exn res in
    match res with
    | None -> Ok (Btyv_prim goal)
    | Some dims -> Ok (Btyv_tensor (goal, dims))
  in

  match dist with
  | D_ber exp ->
    let%bind tyv = tycheck_exp ctxt exp in
    lift [Pty_ureal] Pty_bool [tyv]

  | D_unif ->
    Ok (Btyv_prim Pty_ureal)

  | D_beta (exp1, exp2) ->
    let%bind tyv1 = tycheck_exp ctxt exp1 in
    let%bind tyv2 = tycheck_exp ctxt exp2 in
    lift [Pty_preal; Pty_preal] Pty_ureal [tyv1; tyv2]

  | D_gamma (exp1, exp2) ->
    let%bind tyv1 = tycheck_exp ctxt exp1 in
    let%bind tyv2 = tycheck_exp ctxt exp2 in
    lift [Pty_preal; Pty_preal] Pty_preal [tyv1; tyv2]

  | D_normal (exp1, exp2) ->
    let%bind tyv1 = tycheck_exp ctxt exp1 in
    let%bind tyv2 = tycheck_exp ctxt exp2 in
    lift [Pty_real; Pty_preal] Pty_real [tyv1; tyv2]

  | D_cat exps ->
    let n = List.length exps in
    let%bind () = List.fold_result exps ~init:() ~f:(fun () exp ->
        let%bind tyv = tycheck_exp ctxt exp in
        if is_subtype tyv (Btyv_prim Pty_preal) then
          Ok ()
        else
          Or_error.of_exn (Type_error ("mismatched parameter types", loc))
      )
    in
    Ok (Btyv_prim (Pty_fnat n))

  | D_discrete exp ->
    let%bind tyv = tycheck_exp ctxt exp in
    begin
      match tyv with
      | Btyv_simplex n ->
        Ok (Btyv_prim (Pty_fnat n))
      | _ ->
        Or_error.of_exn (Type_error ("mismatched parameter types", loc))
    end

  | D_bin (n, exp) ->
    let%bind tyv = tycheck_exp ctxt exp in
    lift [Pty_ureal] (Pty_fnat n) [tyv]

  | D_geo exp ->
    let%bind tyv = tycheck_exp ctxt exp in
    lift [Pty_ureal] Pty_nat [tyv]

  | D_pois exp ->
    let%bind tyv = tycheck_exp ctxt exp in
    lift [Pty_preal] Pty_nat [tyv]

let rec eval_sty sty =
  match sty.sty_desc with
  | Sty_one -> Styv_one
  | Sty_conj (ty1, sty2) -> Styv_conj (eval_ty ty1, eval_sty sty2)
  | Sty_imply (ty1, sty2) -> Styv_imply (eval_ty ty1, eval_sty sty2)
  | Sty_ichoice (sty1, sty2) -> Styv_ichoice (eval_sty sty1, eval_sty sty2)
  | Sty_echoice (sty1, sty2) -> Styv_echoice (eval_sty sty1, eval_sty sty2)
  | Sty_var (type_name, None) -> Styv_var (type_name.txt, Styv_one)
  | Sty_var (type_name, Some sty0) -> Styv_var (type_name.txt, eval_sty sty0)

let collect_sess_tys prog =
  Hashtbl.of_alist_or_error (module String) (List.filter_map prog ~f:(fun top ->
      match top with
      | Top_proc _ -> None
      | Top_sess (type_name, sty) -> Some (type_name.txt, Option.map ~f:eval_sty sty)
      | Top_func _ -> None
    ))

let eval_proc_sig psig =
  { psigv_theta_tys = List.map psig.psig_theta_tys ~f:(fun (var_name, pty) -> (var_name.txt, eval_ty pty))
  ; psigv_param_tys = List.map psig.psig_param_tys ~f:(fun (var_name, ty) -> (var_name.txt, eval_ty ty))
  ; psigv_ret_ty = eval_ty psig.psig_ret_ty
  ; psigv_sess_left = Option.map psig.psig_sess_left ~f:(fun (channel_name, type_name) -> (channel_name.txt, type_name.txt))
  ; psigv_sess_right = Option.map psig.psig_sess_right ~f:(fun (channel_name, type_name) -> (channel_name.txt, type_name.txt))
  }

let collect_proc_sigs prog =
  String.Map.of_alist_or_error (List.filter_map prog ~f:(fun top ->
      match top with
      | Top_sess _ -> None
      | Top_proc (proc_name, { proc_sig; _ }) -> Some (proc_name.txt, eval_proc_sig proc_sig)
      | Top_func _ -> None
    ))

let collect_func_sigs prog =
  String.Map.of_alist_or_error (List.filter_map prog ~f:(fun top ->
      match top with
      | Top_sess _ -> None
      | Top_proc _ -> None
      | Top_func (func_name, { func_param_tys ; func_ret_ty; _ }) -> Some (func_name.txt, Btyv_arrow (
          (match func_param_tys with
           | [] -> Btyv_prim (Pty_unit)
           | [(_, ty)] -> eval_ty ty
           | _ -> Btyv_product (List.map func_param_tys ~f:(fun (_, ty) -> eval_ty ty))),
          eval_ty func_ret_ty
        ))
    ))

let tycheck_cmd psig_ctxt =
  let rec forward ctxt cmd =
    match cmd.cmd_desc with
    | M_ret exp ->
      tycheck_exp ctxt exp
    | M_bnd (cmd1, var_name, cmd2) ->
      let%bind tyv1 = forward ctxt cmd1 in
      let ctxt' = match var_name with
        | None -> ctxt
        | Some var_name ->
          update_ctx ctxt ~key:var_name.txt ~data:tyv1
      in
      forward ctxt' cmd2
    | M_call (proc_name, exps) ->
      begin
        match Map.find psig_ctxt proc_name.txt with
        | None -> Or_error.of_exn (Type_error ("unknown procedure " ^ proc_name.txt, proc_name.loc))
        | Some psigv ->
          if List.length psigv.psigv_param_tys <> List.length exps then
            Or_error.of_exn (Type_error ("mismatched arity", cmd.cmd_loc))
          else
            let%bind tyvs = List.fold_result (List.rev exps) ~init:[] ~f:(fun acc exp ->
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
      begin
        match tyv with
        | Btyv_prim Pty_bool ->
          let%bind tyv1 = forward ctxt cmd1 in
          let%bind tyv2 = forward ctxt cmd2 in
          join_type ~loc:cmd.cmd_loc tyv1 tyv2
        | _ ->
          Or_error.of_exn (Type_error ("non-boolean condition type", exp.exp_loc))
      end
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
    | M_bnd (cmd1, var_name, cmd2) ->
      let%bind tyv1 = forward ctxt cmd1 in
      let ctxt' =
        match var_name with
        | None -> ctxt
        | Some var_name ->
          update_ctx ctxt ~key:var_name.txt ~data:tyv1
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
    | M_call (proc_name, _) ->
      begin
        match Map.find psig_ctxt proc_name.txt with
        | None -> Or_error.of_exn (Type_error ("unknown procedure " ^ proc_name.txt, proc_name.loc))
        | Some psigv ->
          let%bind sess0 = String.Map.of_alist_or_error
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
    let%bind sess = String.Map.of_alist_or_error (List.append (Option.to_list sess_left) (Option.to_list sess_right)) in
    let%bind sess' = backward ctxt sess cmd in
    Ok (tyv,
        Option.map sess_left ~f:(fun (channel_id, _) -> let (_, sty) = Map.find_exn sess' channel_id in (channel_id, sty)),
        Option.map sess_right ~f:(fun (channel_id, _) -> let (_, sty) = Map.find_exn sess' channel_id in (channel_id, sty))
       )

let tycheck_func func_ctxt func =
  let%bind ctxt = String.Map.of_alist_or_error
      (List.concat
         [ Map.to_alist func_ctxt
         ; (List.map func.func_param_tys ~f:(fun (var_name, ty) -> (var_name.txt, eval_ty ty)))]) in
  let%bind ret_tyv = tycheck_exp (String.Map.empty, ctxt) func.func_body in
  let decl_tyv = eval_ty func.func_ret_ty in
  if equal_base_tyv ret_tyv decl_tyv then
    Ok ()
  else
    Or_error.of_exn (Type_error ("incorrect function return type", func.func_loc))

let tycheck_proc sty_ctxt psig_ctxt func_ctxt proc =
  let psigv = eval_proc_sig proc.proc_sig in
  let%bind ctxt = String.Map.of_alist_or_error
      (List.concat
         [ Map.to_alist func_ctxt
         ; (List.map psigv.psigv_theta_tys ~f:(fun (var_name, pty) -> (var_name, pty)))
         ; psigv.psigv_param_tys]) in
  let%bind (tyv, sess_left, sess_right) = tycheck_cmd psig_ctxt (String.Map.empty, ctxt)
      (Option.map psigv.psigv_sess_left ~f:(fun (channel_id, _) -> (channel_id, Styv_one)))
      (Option.map psigv.psigv_sess_right ~f:(fun (channel_id, _) -> (channel_id, Styv_one)))
      proc.proc_body in
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
  | Sty_var (type_name, sty0) ->
    match Hashtbl.find sty_ctxt type_name.txt with
    | None -> Or_error.of_exn (Type_error ("unknown type " ^ type_name.txt, type_name.loc))
    | Some _ -> Option.value_map sty0 ~default:(Ok ()) ~f:(verify_sess_ty sty_ctxt)

let tycheck_prog prog =
  let%bind sty_ctxt = collect_sess_tys prog in
  let%bind psig_ctxt = collect_proc_sigs prog in
  let%bind func_ctxt = collect_func_sigs prog in
  List.fold_result prog ~init:() ~f:(fun () top ->
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

let () =
  Location.register_error_of_exn
    (function
      | Type_error (msg, loc) -> Some (Location.errorf ~loc "%s" msg)
      | _ -> None
    )
