type 'a t = <
  sample: unit -> 'a;
  log_prob: 'a -> Tensor.t;
>

val bernoulli : Tensor.t -> Tensor.t t

val normal : Tensor.t -> Tensor.t -> Tensor.t t
