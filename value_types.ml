open Core
open Ast_types

type value =
  | Val_triv
  | Val_bool of bool
  | Val_real of float
  | Val_int of int
  | Val_abs of string * exp * closure
  | Val_prim_func of (value -> value Or_error.t)
  | Val_dist of value Dist.t
  | Val_tensor of Tensor.t
  | Val_tuple of value list

and closure = value String.Map.t

type fancy_value =
  | Fval_base of value
  | Fval_poly of (int list -> value option)
