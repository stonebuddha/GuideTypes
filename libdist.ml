open Core
open Ast_types
open Value_types

let bad_impl = Utils.bad_implementation

(* Type Specs *)

let ft_gen args ret =
  Ftyv_poly
    (fun dims -> Some (Btyv_arrow (
         (match args with
          | [] -> Btyv_unit
          | [arg] -> Btyv_tensor (arg, dims)
          | _ -> Btyv_product (List.map args ~f:(fun arg -> Btyv_tensor (arg, dims)))),
         Btyv_dist (Btyv_tensor (ret, dims))
       )))

let ft_ber = ft_gen [Pty_ureal] Pty_bool

let ft_unif = ft_gen [] Pty_ureal

let ft_beta = ft_gen [Pty_preal; Pty_preal] Pty_ureal

let ft_gamma = ft_gen [Pty_preal; Pty_preal] Pty_preal

let ft_normal = ft_gen [Pty_real; Pty_preal] Pty_real

let ft_cat = Ftyv_poly (fun dims ->
    match dims with
    | [] -> None
    | n :: dims' ->
      Some (Btyv_arrow (Btyv_tensor (Pty_preal, dims), Btyv_dist (Btyv_tensor (Pty_fnat n, dims'))))
  )

let ft_disc = Ftyv_poly (fun dims ->
    match dims with
    | [n] -> Some (Btyv_arrow (Btyv_simplex n, Btyv_dist (Btyv_tensor (Pty_fnat n, []))))
    | _ -> None
  )

let ft_bin = Ftyv_poly (fun dims ->
    match dims with
    | [] -> None
    | n :: dims' ->
      Some (Btyv_arrow (Btyv_tensor (Pty_ureal, dims'), Btyv_dist (Btyv_tensor (Pty_fnat (n + 1), dims'))))
  )

let ft_geo = ft_gen [Pty_ureal] Pty_nat

let ft_pois = ft_gen [Pty_preal] Pty_nat

let ft_dirich = Ftyv_poly (fun dims ->
    match dims with
    | [n] -> Some (Btyv_arrow (Btyv_tensor (Pty_preal, [n]), Btyv_dist (Btyv_simplex n)))
    | _ -> None
  )

let ft_wishart = Ftyv_poly (fun dims ->
    match dims with
    | [n] -> Some (Btyv_arrow (Btyv_product [Btyv_tensor (Pty_real, [n; n]); Btyv_tensor (Pty_preal, [])], Btyv_dist (Btyv_tensor (Pty_real, [n; n]))))
    | _ -> None
  )

let ft_mvn = Ftyv_poly (fun dims ->
    match dims with
    | [n] -> Some (Btyv_arrow (Btyv_product [Btyv_tensor (Pty_real, [n]); Btyv_tensor (Pty_real, [n; n])], Btyv_dist (Btyv_tensor (Pty_real, [n]))))
    | _ -> None
  )

let prelude = String.Map.of_alist_exn [
    "ber", ft_ber;
    "unif", ft_unif;
    "beta", ft_beta;
    "gamma", ft_gamma;
    "normal", ft_normal;
    "cat", ft_cat;
    "disc", ft_disc;
    "bin", ft_bin;
    "geo", ft_geo;
    "pois", ft_pois;
    "dirich", ft_dirich;
    "wishart", ft_wishart;
    "mvn", ft_mvn;
  ]

(* Library functions *)

let pf_gen1 pf name = Fval_poly
    (fun _ ->
       Some (Val_prim_func (function
           | Val_tensor t -> Ok (Val_dist (pf t))
           | _ -> bad_impl ("pf_" ^ name)
         ))
    )

let pf_gen2 pf name = Fval_poly
    (fun _ ->
       Some (Val_prim_func (function
           | Val_tuple [Val_tensor t0; Val_tensor t1] -> Ok (Val_dist (pf t0 t1))
           | _ -> bad_impl ("pf_" ^ name)
         ))
    )

let pf_ber = pf_gen1 (fun t -> Dist.bernoulli t) "ber"

let pf_normal = pf_gen2 (fun t0 t1 -> Dist.normal t0 t1) "normal"

let pf_gamma = pf_gen2 (fun t0 t1 -> Dist.gamma t0 t1) "gamma"

let pf_unif = Fval_poly
    (fun dims ->
       Some (Val_prim_func (function
           | Val_triv -> Ok (Val_dist (Dist.unif dims))
           | _ -> bad_impl "pf_dist"
         ))
    )

let pf_beta = pf_gen2 (fun t0 t1 -> Dist.beta t0 t1) "beta"

let stdlib = String.Map.of_alist_exn [
    "ber", pf_ber;
    "unif", pf_unif;
    "beta", pf_beta;
    "gamma", pf_gamma;
    "normal", pf_normal;
  ]
