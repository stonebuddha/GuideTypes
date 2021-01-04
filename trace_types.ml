type event =
  | Ev_tensor_left of Tensor.t
  | Ev_tensor_right of Tensor.t
  | Ev_branch_left of bool
  | Ev_branch_right of bool

type trace = event list
