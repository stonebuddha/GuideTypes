open Core
open Ctypes
open Foreign
open Torch
open Ast_types

include Tensor

(* Stubs *)

let stub_t : t typ = Obj.magic (ptr void)

let stub_bool_vec = foreign "at_bool_vec" (ptr bool @-> int @-> int @-> returning stub_t)

let stub_bool_value = foreign "at_bool_value_at_indexes" (stub_t @-> ptr int @-> int @-> returning bool)

let stub_free = foreign "at_free" (stub_t @-> returning void)

(* Extensions *)

let bool_vec values =
  let values_len = List.length values in
  let values = CArray.of_list bool values |> CArray.start in
  let kind = Torch_core.Kind.T Torch_core.Kind.Bool in
  let t = stub_bool_vec values values_len (Torch_core.Kind.packed_to_int kind) in
  Gc.Expert.add_finalizer (Heap_block.create_exn t) (fun block -> stub_free (Heap_block.value block));
  t

let bool_get t indexes =
  stub_bool_value t (CArray.of_list int indexes |> CArray.start) (List.length indexes)

let mk_f = f

let mk_i v = int_vec [v] |> reshape ~shape:[]

let mk_b v = bool_vec [v] |> reshape ~shape:[]

let ( <> ) = Torch_core.Wrapper.Tensor.ne1

let ( < ) = Torch_core.Wrapper.Tensor.lt1

let ( <= ) = Torch_core.Wrapper.Tensor.le1

let ( > ) = Torch_core.Wrapper.Tensor.gt1

let ( >= ) = Torch_core.Wrapper.Tensor.ge1

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
