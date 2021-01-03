open Core
open Ast_types

(* Type Specs *)

let ft_gen args ret =
  Ftyv_base (Btyv_arrow (
      (match args with
       | [] -> Btyv_unit
       | [arg] -> Btyv_prim arg
       | _ -> Btyv_product (List.map args ~f:(fun arg -> Btyv_prim arg))),
      Btyv_dist (Btyv_prim ret)))

let ft_gen_tensor args ret =
  Ftyv_poly
    (fun dims -> Some (Btyv_arrow (
         (match args with
          | [] -> Btyv_unit
          | [arg] -> Btyv_tensor (arg, dims)
          | _ -> Btyv_product (List.map args ~f:(fun arg -> Btyv_tensor (arg, dims)))),
         Btyv_dist (Btyv_tensor (ret, dims))
       )))

let ft_ber = ft_gen [Pty_ureal] Pty_bool

let ft_bers = ft_gen_tensor [Pty_ureal] Pty_bool

let ft_unif = ft_gen [] Pty_ureal

let ft_unifs = ft_gen_tensor [] Pty_ureal

let ft_beta = ft_gen [Pty_preal; Pty_preal] Pty_ureal

let ft_betas = ft_gen_tensor [Pty_preal; Pty_preal] Pty_ureal

let ft_gamma = ft_gen [Pty_preal; Pty_preal] Pty_preal

let ft_gammas = ft_gen_tensor [Pty_preal; Pty_preal] Pty_preal

let ft_normal = ft_gen [Pty_real; Pty_preal] Pty_real

let ft_normals = ft_gen_tensor [Pty_real; Pty_preal] Pty_real

let ft_cat = Ftyv_poly (fun dims ->
    match dims with
    | [n] -> Some (Btyv_arrow (Btyv_tensor (Pty_preal, [n]), Btyv_dist (Btyv_prim (Pty_fnat n))))
    | _ -> None
  )

let ft_cats = Ftyv_poly (fun dims ->
    match dims with
    | [] -> None
    | _ ->
      let dims', n = List.drop_last_exn dims, List.last_exn dims in
      Some (Btyv_arrow (Btyv_tensor (Pty_preal, dims), Btyv_dist (Btyv_tensor (Pty_fnat n, dims'))))
  )

let ft_disc = Ftyv_poly (fun dims ->
    match dims with
    | [n] -> Some (Btyv_arrow (Btyv_simplex n, Btyv_dist (Btyv_prim (Pty_fnat n))))
    | _ -> None
  )

let ft_bin = Ftyv_poly (fun dims ->
    match dims with
    | [n] -> Some (Btyv_arrow (Btyv_prim Pty_ureal, Btyv_dist (Btyv_prim (Pty_fnat (n + 1)))))
    | _ -> None
  )

let ft_bins = Ftyv_poly (fun dims ->
    match dims with
    | [] -> None
    | _ ->
      let dims', n = List.drop_last_exn dims, List.last_exn dims in
      Some (Btyv_arrow (Btyv_tensor (Pty_ureal, dims'), Btyv_dist (Btyv_tensor (Pty_fnat (n + 1), dims'))))
  )

let ft_geo = ft_gen [Pty_ureal] Pty_nat

let ft_geos = ft_gen_tensor [Pty_ureal] Pty_nat

let ft_pois = ft_gen [Pty_preal] Pty_nat

let ft_poiss = ft_gen_tensor [Pty_preal] Pty_nat

let ft_dirich = Ftyv_poly (fun dims ->
    match dims with
    | [n] -> Some (Btyv_arrow (Btyv_tensor (Pty_preal, [n]), Btyv_dist (Btyv_simplex n)))
    | _ -> None
  )

let ft_wishart = Ftyv_poly (fun dims ->
    match dims with
    | [n] -> Some (Btyv_arrow (Btyv_product [Btyv_tensor (Pty_real, [n; n]); Btyv_prim Pty_preal], Btyv_dist (Btyv_tensor (Pty_real, [n; n]))))
    | _ -> None
  )

let ft_mvn = Ftyv_poly (fun dims ->
    match dims with
    | [n] -> Some (Btyv_arrow (Btyv_product [Btyv_tensor (Pty_real, [n]); Btyv_tensor (Pty_real, [n; n])], Btyv_dist (Btyv_tensor (Pty_real, [n]))))
    | _ -> None
  )

let prelude = String.Map.of_alist_exn [
    "ber", ft_ber;
    "bers", ft_bers;
    "unif", ft_unif;
    "unifs", ft_unifs;
    "beta", ft_beta;
    "betas", ft_betas;
    "gamma", ft_gamma;
    "gammas", ft_gammas;
    "normal", ft_normal;
    "normals", ft_normals;
    "cat", ft_cat;
    "cats", ft_cats;
    "disc", ft_disc;
    "bin", ft_bin;
    "bins", ft_bins;
    "geo", ft_geo;
    "geos", ft_geos;
    "pois", ft_pois;
    "poiss", ft_poiss;
    "dirich", ft_dirich;
    "wishart", ft_wishart;
    "mvn", ft_mvn;
  ]

(* Library functions *)
