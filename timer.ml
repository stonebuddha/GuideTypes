open Core

let wrap_duration name f =
  let t1 = Time_now.nanoseconds_since_unix_epoch () |> Time_ns.of_int63_ns_since_epoch in
  Exn.protect ~f ~finally:(fun () ->
      let t2 = Time_now.nanoseconds_since_unix_epoch () |> Time_ns.of_int63_ns_since_epoch in
      Format.printf "%s time: %a@." name Time_ns.Span.pp (Time_ns.diff t2 t1)
    )
