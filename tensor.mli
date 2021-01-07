include module type of Torch.Tensor

val kind : t -> [ `Float | `Int | `Bool ]

val mk_f : float -> t

val mk_i : int -> t

val mk_b : bool -> t

val bool_get : t -> int list -> bool

val bool_value : t -> bool

val ( <> ) : t -> t -> t

val ( < ) : t -> t -> t

val ( <= ) : t -> t -> t

val ( > ) : t -> t -> t

val ( >= ) : t -> t -> t

val eye : ?requires_grad:bool -> ?kind:Torch_core.Kind.packed -> ?device:Torch.Device.t -> ?scale:float -> int -> t

val normal2 : mean:t -> std:t -> t

val sum1 : dim:int list -> ?keepdim:bool -> ?dtype:Torch_core.Kind.packed -> t -> t

val dirichlet : t -> t
