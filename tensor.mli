include module type of Torch.Tensor

val mk_f : float -> t

val mk_i : int -> t

val mk_b : bool -> t

val bool_get : t -> int list -> bool

val ( <> ) : t -> t -> t

val ( < ) : t -> t -> t

val ( <= ) : t -> t -> t

val ( > ) : t -> t -> t

val ( >= ) : t -> t -> t

val eye : int -> t
