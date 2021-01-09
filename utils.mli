open Core

val wrap_duration : string -> (unit -> 'a) -> 'a

val fold_right_result : 'a list -> f:('a -> 'accum -> 'accum Or_error.t) -> init:'accum -> 'accum Or_error.t

val bad_implementation : string -> 'a Or_error.t

val float_eps : float

val float_tiny : float
