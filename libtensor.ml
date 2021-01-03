open Core
open Ast_types
open Value_types

let bad_impl = Utils.bad_implementation

(* Type Specs *)

let ft_gen0 pty = Ftyv_poly
    (fun dims -> Some (Btyv_arrow (Btyv_unit, Btyv_tensor (pty, dims))))

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

(* Library functions *)

let pf_gen0 pf name = Fval_poly
    (fun dims ->
       Some (Val_prim_func (function
           | Val_triv -> Ok (Val_tensor (pf dims))
           | _ -> bad_impl ("pf_" ^ name)
         ))
    )

let pf_gen1 pf name = Fval_poly
    (fun _ ->
       Some (Val_prim_func (function
           | Val_tensor t -> Ok (Val_tensor (pf t))
           | _ -> bad_impl ("pf_" ^ name)
         ))
    )

let pf_zeros = pf_gen0 Tensor.zeros "zeros"

let pf_ones = pf_gen0 Tensor.ones "ones"

let pf_mulmm = Fval_poly
    (fun _ ->
       Some (Val_prim_func (function
           | Val_tuple [Val_tensor t1; Val_tensor t2] -> Ok (Val_tensor (Tensor.mm t1 t2))
           | _ -> bad_impl "pf_mulmm"
         ))
    )

let pf_softplus = pf_gen1 Tensor.softplus "softplus"

let pf_sigmoid = pf_gen1 Tensor.sigmoid "sigmoid"

let pf_exp = pf_gen1 Tensor.exp "exp"

let stdlib = String.Map.of_alist_exn [
    "zeros", pf_zeros;
    "ones", pf_ones;
    "mulmv", pf_mulmm;
    "mulvm", pf_mulmm;
    "mulmm", pf_mulmm;
    "softplus", pf_softplus;
    "sigmoid", pf_sigmoid;
    "exp", pf_exp;
  ]
