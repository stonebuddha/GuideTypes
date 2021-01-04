open Core
open Trace_types

let print_tensor fmt t =
  let dims = Tensor.shape t in
  let kind =
    match Tensor.kind t with
    | Torch_core.Kind.(T Half)
    | Torch_core.Kind.(T Float)
    | Torch_core.Kind.(T Double) -> `Float
    | Torch_core.Kind.(T Uint8)
    | Torch_core.Kind.(T Int8)
    | Torch_core.Kind.(T Int16)
    | Torch_core.Kind.(T Int)
    | Torch_core.Kind.(T Int64) -> `Int
    | Torch_core.Kind.(T Bool) -> `Bool
    | _ -> assert false
  in
  let rec aux dims t =
    match dims with
    | [] ->
      begin
        match kind with
        | `Float ->
          let f = Tensor.float_value t in
          Format.fprintf fmt "%g" f
        | `Int ->
          let n = Tensor.int_value t in
          Format.fprintf fmt "%d" n
        | `Bool ->
          let b = Tensor.bool_value t in
          Format.fprintf fmt (if b then "true" else "false")
      end
    | n :: dims' ->
      begin
        Format.fprintf fmt "[| ";
        for i = 0 to n - 1 do
          if i = 0 then Format.fprintf fmt "; ";
          aux dims' (Tensor.get t i)
        done;
        Format.fprintf fmt " |]"
      end
  in
  aux dims t

let print_event fmt = function
  | Ev_branch_left br
  | Ev_branch_right br ->
    Format.fprintf fmt (if br then "(true)" else "(false)")
  | Ev_tensor_left t
  | Ev_tensor_right t ->
    print_tensor fmt t

let print_trace fmt tr =
  Format.fprintf fmt "[ ";
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ") print_event fmt tr;
  Format.fprintf fmt " ]"
