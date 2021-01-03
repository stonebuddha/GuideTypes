open Core

val wrap_duration : string -> (unit -> 'a) -> 'a

val fold_right_result : 'a list -> f:('a -> 'accum -> 'accum Or_error.t) -> init:'accum -> 'accum Or_error.t
