include module type of Torch.Tensor

type kind =
  | Float (* Float32 *)
  | Int (* Int32 *)
  | Bool

val kind : t -> kind

val to_type : t -> type_:kind -> t

val bool_vec : bool list -> t

val bool_get : t -> int list -> bool

val bool_value : t -> bool

val mk_f : float -> t

val mk_i : int -> t

val mk_b : bool -> t

val ( <> ) : t -> t -> t

val ( < ) : t -> t -> t

val ( <= ) : t -> t -> t

val ( > ) : t -> t -> t

val ( >= ) : t -> t -> t

val eye : ?requires_grad:bool -> ?kind:kind -> ?device:Torch.Device.t -> ?scale:float -> int -> t

val normal2 : mean:t -> std:t -> t

val sum1 : dim:int list -> ?keepdim:bool -> ?dtype:kind -> t -> t

val dirichlet : t -> t

val binary_cross_entropy_with_logits :
  target:t -> ?weight:t -> ?pos_weight:t -> ?reduction:[ `None | `Elementwise_mean | `Sum ] -> t -> t

val rand : ?kind:kind -> int list -> t

val arange : ?options:kind * Torch.Device.t -> 'a Torch.Scalar.t -> t

val arange1 : ?options:kind * Torch.Device.t -> start:'a Torch.Scalar.t -> 'a Torch.Scalar.t -> t
