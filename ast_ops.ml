open Core
open Ast_types

let print_prim_ty fmt = function
  | Pty_unit -> Format.fprintf fmt "unit"
  | Pty_bool -> Format.fprintf fmt "bool"
  | Pty_ureal -> Format.fprintf fmt "ureal"
  | Pty_preal -> Format.fprintf fmt "preal"
  | Pty_real -> Format.fprintf fmt "real"
  | Pty_fnat n -> Format.fprintf fmt "nat[%d]" n
  | Pty_nat -> Format.fprintf fmt "nat"

let rec print_base_tyv fmt = function
  | Btyv_arrow (tyv1, tyv2) ->
    Format.fprintf fmt "%a -> %a" print_base_tyv_prim tyv1 print_base_tyv tyv2
  | tyv -> print_base_tyv_prim fmt tyv

and print_base_tyv_prim fmt = function
  | Btyv_prim pty ->
    print_prim_ty fmt pty
  | Btyv_dist tyv ->
    Format.fprintf fmt "%a dist" print_base_tyv_prim tyv
  | Btyv_tensor (pty, dims) ->
    Format.fprintf fmt "(%a; [%a]) tensor" print_prim_ty pty
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ") Int.pp) dims
  | tyv ->
    Format.fprintf fmt "(%a)" print_base_tyv tyv

let rec print_sess_tyv fmt = function
  | Styv_one ->
    Format.fprintf fmt "$"
  | Styv_conj (tyv1, styv2) ->
    Format.fprintf fmt "%a * %a" print_base_tyv tyv1 print_sess_tyv styv2
  | Styv_imply (tyv1, styv2) ->
    Format.fprintf fmt "%a -o %a" print_base_tyv tyv1 print_sess_tyv styv2
  | Styv_ichoice (styv1, styv2) ->
    Format.fprintf fmt "+{ %a | %a }" print_sess_tyv styv1 print_sess_tyv styv2
  | Styv_echoice (styv1, styv2) ->
    Format.fprintf fmt "&{ %a | %a }" print_sess_tyv styv1 print_sess_tyv styv2
  | Styv_var (type_name, styv0) ->
    Format.fprintf fmt "%s[%a]" type_name print_sess_tyv styv0
