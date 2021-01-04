val wrap_duration : string -> (unit -> 'a) -> 'a

val fold_right_result : 'a list -> f:('a -> 'accum -> 'accum Core.Or_error.t) -> init:'accum -> 'accum Core.Or_error.t

val bad_implementation : string -> 'a Core.Or_error.t
