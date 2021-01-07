open Core
open Trace_types

let print_tensor fmt t =
  let dims = Tensor.shape t in
  let kind = Tensor.(kind t) in
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

let create_system spec trace =
  let (model_c, model_th, model_l, model_r) = spec.sys_spec_model in
  let (guide_c, guide_th, guide_l, guide_r) = spec.sys_spec_guide in
  { sys_buffer = String.Map.of_alist_exn [spec.sys_spec_input_channel, Queue.of_list trace; spec.sys_spec_output_channel, Queue.create ()]
  ; sys_model = { subr_log_prob_sum = Tensor.f 0.
                ; subr_env = String.Map.empty
                ; subr_cont = Either.first model_c, Cont_stop
                ; subr_channel_left = model_l
                ; subr_channel_right = model_r
                }
  ; sys_guide = { subr_log_prob_sum = Tensor.f 0.
                ; subr_env = String.Map.empty
                ; subr_cont = Either.first guide_c, Cont_stop
                ; subr_channel_left = guide_l
                ; subr_channel_right = guide_r
                }
  ; sys_input_channel = spec.sys_spec_input_channel
  ; sys_output_channel = spec.sys_spec_output_channel
  },
  model_th,
  guide_th

let rec py_tensor t =
  let dims = Tensor.shape t in
  let kind = Tensor.kind t in
  match dims with
  | [] ->
    begin
      match kind with
      | `Float -> Py.Float.of_float (Tensor.float_value t)
      | `Int -> Py.Int.of_int (Tensor.int_value t)
      | `Bool -> Py.Bool.of_bool (Tensor.bool_value t)
    end
  | _ ->
    let ts = Tensor.to_list t in
    Py.List.of_list_map py_tensor ts

let py_event = function
  | Ev_branch_left br
  | Ev_branch_right br ->
    Py.Tuple.of_pair (Py.String.of_string "B", Py.Bool.of_bool br)
  | Ev_tensor_left t
  | Ev_tensor_right t ->
    let tobj = py_tensor t in
    Py.Tuple.of_pair (Py.String.of_string "T", tobj)

let py_trace tr =
  Py.List.of_list_map py_event tr
