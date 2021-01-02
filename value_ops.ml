open Core
open Value_types

let fval_to_base_exn = function
  | Fval_base value -> value
  | Fval_poly _ -> raise Error.(to_exn (of_string "fval_to_base_exn (Fval_poly _)"))

let fval_to_poly_exn = function
  | Fval_poly func -> func
  | Fval_base _ -> raise Error.(to_exn (of_string "fval_to_poly_exn (Fval_base _)"))
