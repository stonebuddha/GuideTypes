type 'a t = <
  rsample: (unit -> 'a) option;
  sample: unit -> 'a;
  log_prob: 'a -> Tensor.t;
>

val bernoulli : Tensor.t -> Tensor.t t

val normal : Tensor.t -> Tensor.t -> Tensor.t t

val gamma : Tensor.t -> Tensor.t -> Tensor.t t

val unif : int list -> Tensor.t t

val beta : Tensor.t -> Tensor.t -> Tensor.t t

val binomial : int -> Tensor.t -> Tensor.t t

val categorical : Tensor.t -> Tensor.t t

val geometric : Tensor.t -> Tensor.t t

val poisson : Tensor.t -> Tensor.t t

val dirichlet : Tensor.t -> Tensor.t t

val multivariate_normal : Tensor.t -> Tensor.t -> Tensor.t t

val lkj_cholesky : int -> Tensor.t -> Tensor.t t
