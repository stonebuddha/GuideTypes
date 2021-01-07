open Core
open Ctypes
open Foreign
open Torch

include Tensor

(* Stubs *)

let stub_t : t typ = Obj.magic (ptr void)

let stub_bool_vec = foreign "at_bool_vec" (ptr bool @-> int @-> int @-> returning stub_t)

let stub_bool_value = foreign "at_bool_value_at_indexes" (stub_t @-> ptr int @-> int @-> returning bool)

let stub_free = foreign "at_free" (stub_t @-> returning void)

let stub_dirichlet = foreign "at_dirichlet" (stub_t @-> returning stub_t)

(* Extensions *)

let kind t =
  match kind t with
  | T Half
  | T Double
  | T Float -> `Float
  | T Uint8
  | T Int8
  | T Int16
  | T Int
  | T Int64 -> `Int
  | T Bool -> `Bool
  | _ -> assert false

let bool_vec values =
  let values_len = List.length values in
  let values = CArray.of_list bool values |> CArray.start in
  let kind = Torch_core.Kind.(T Bool) in
  let t = stub_bool_vec values values_len (Torch_core.Kind.packed_to_int kind) in
  Gc.Expert.add_finalizer (Heap_block.create_exn t) (fun block -> stub_free (Heap_block.value block));
  t

let bool_get t indexes =
  stub_bool_value t (CArray.of_list int indexes |> CArray.start) (List.length indexes)

let bool_value t =
  stub_bool_value t (from_voidp int null) 0

let mk_f = f

let mk_i v = int_vec [v] |> reshape ~shape:[]

let mk_b v = bool_vec [v] |> reshape ~shape:[]

let ( <> ) = ne1

let ( < ) = lt1

let ( <= ) = le1

let ( > ) = gt1

let ( >= ) = ge1

let eye
    ?(requires_grad=false)
    ?(kind=Torch_core.Kind.T Float)
    ?(device=Device.Cpu)
    ?scale
    n =
  let t = eye ~n ~options:(kind, device) in
  let t =
    Option.value_map scale ~default:t ~f:(fun scale -> t * (float_vec [scale] ~device))
  in
  if requires_grad then set_requires_grad t ~r:true else t

let normal2 ~mean ~std =
  let res = Tensor.zeros (shape mean) in
  Tensor.normal_out2 ~out:res ~mean ~std

let sum1 ~dim ?(keepdim=false) ?(dtype=Torch_core.Kind.T Float) t = sum1 t ~dim ~keepdim ~dtype

let dirichlet = stub_dirichlet
