open Core
open Or_error.Let_syntax

let wrap_duration name f =
  let t1 = Time_now.nanoseconds_since_unix_epoch () |> Time_ns.of_int63_ns_since_epoch in
  let res = f () in
  let t2 = Time_now.nanoseconds_since_unix_epoch () |> Time_ns.of_int63_ns_since_epoch in
  Format.printf "%s time: %a@." name Time_ns.Span.pp (Time_ns.diff t2 t1);
  res

let fold_right_result lst ~f ~init =
  let rec inner lst =
    match lst with
    | [] -> Ok init
    | h :: t ->
      let%bind res = inner t in
      f h res
  in
  inner lst

let bad_implementation msg =
  Or_error.error_string ("bad implementation: " ^ msg)
