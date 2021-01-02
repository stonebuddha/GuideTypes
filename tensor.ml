open Core
(* open Torch *)
open Ast_types

let ft_gen0 pty = Ftyv_poly
    (fun dims -> Some (Btyv_arrow (Btyv_prim Pty_unit, Btyv_tensor (pty, dims))))

let ft_gen1 pty1 pty2 = Ftyv_poly
    (fun dims -> Some (Btyv_arrow (Btyv_tensor (pty1, dims), Btyv_tensor (pty2, dims))))

let ft_zeros = ft_gen0 (Pty_fnat 1)

let ft_ones = ft_gen0 (Pty_fnat 2)

let ft_mulmv = Ftyv_poly
    (fun dims ->
       match dims with
       | [a; b] -> Some (Btyv_arrow (Btyv_product [Btyv_tensor (Pty_real, [a; b]); Btyv_tensor (Pty_real, [b])], Btyv_tensor (Pty_real, [a])))
       | _ -> None
    )

let ft_mulvm = Ftyv_poly
    (fun dims ->
       match dims with
       | [a; b] -> Some (Btyv_arrow (Btyv_product [Btyv_tensor (Pty_real, [a]); Btyv_tensor (Pty_real, [a; b])], Btyv_tensor (Pty_real, [b])))
       | _ -> None
    )

let ft_mulmm = Ftyv_poly
    (fun dims ->
       match dims with
       | [a; b; c] -> Some (Btyv_arrow (Btyv_product [Btyv_tensor (Pty_real, [a; b]); Btyv_tensor (Pty_real, [b; c])], Btyv_tensor (Pty_real, [a; c])))
       | _ -> None
    )

let ft_softplus = ft_gen1 Pty_real Pty_preal

let ft_sigmoid = ft_gen1 Pty_real Pty_ureal

let ft_exp = ft_gen1 Pty_real Pty_preal

let prelude = String.Map.of_alist_exn [
    "zeros", ft_zeros;
    "ones", ft_ones;
    "mulmv", ft_mulmv;
    "mulvm", ft_mulvm;
    "mulmm", ft_mulmm;
    "softplus", ft_softplus;
    "sigmoid", ft_sigmoid;
    "exp", ft_exp;
  ]
