open Core

type 'a t = <
  sample: unit -> 'a;
  log_prob: 'a -> Tensor.t;
>

let clamp_probs probs =
  Tensor.clamp probs ~min:(Torch.Scalar.float 1e-8) ~max:(Torch.Scalar.float Float.(1. - 1e-8))

let probs_to_logits ~is_binary probs =
  let ps_clamped = clamp_probs probs in
  if is_binary then
    Tensor.(log ps_clamped - log1p (- ps_clamped))
  else
    Tensor.log ps_clamped

let bernoulli probs : Tensor.t t =
  let lazy logits = lazy (probs_to_logits ~is_binary:true probs) in
  object
    method sample () =
      Tensor.no_grad (fun () ->
          let t = Tensor.bernoulli probs in
          Tensor.to_type t ~type_:Torch_core.Kind.(T Bool)
        )

    method log_prob t =
      Tensor.(- (binary_cross_entropy_with_logits logits ~target:t ~weight:None ~pos_weight:None ~reduction:(Torch_core.Reduction.None)))
  end

let normal loc scale : Tensor.t t =
  let lazy variance = lazy (Tensor.square scale) in
  let lazy log_scale = lazy (Tensor.log scale) in
  let lazy shift = lazy Float.(log (sqrt (2. * pi))) in
  object
    method sample () =
      Tensor.no_grad (fun () ->
          Tensor.normal2 ~mean:loc ~std:scale
        )

    method log_prob t =
      Tensor.((- (square (t - loc))) / (scale variance 2.) - log_scale - f shift)
  end
