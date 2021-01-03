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

let eye n = eye ~n ~options:(Torch_core.Kind.(T Float), Device.Cpu)
