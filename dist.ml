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
  let loc = Tensor.(to_type loc ~type_:Torch_core.Kind.(T Float)) in
  let scale = Tensor.to_type scale ~type_:Torch_core.Kind.(T Float) in
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

let gamma concentration rate : Tensor.t t =
  let concentration = Tensor.(to_type concentration ~type_:Torch_core.Kind.(T Float)) in
  let rate = Tensor.(to_type rate ~type_:Torch_core.Kind.(T Float)) in
  object
    method sample () =
      Tensor.no_grad (fun () ->
          let value = Tensor.(_standard_gamma concentration / rate) in
          let value_detached = Tensor.detach value in
          Tensor.clamp_min value_detached ~min:(Torch.Scalar.float 1e-8)
        )

    method log_prob t =
      Tensor.(concentration * log rate + (concentration - f 1.) * log t - rate * t - lgamma concentration)
  end

let unif dims : Tensor.t t =
  object
    method sample () =
      Tensor.no_grad (fun () ->
          Tensor.rand ~kind:(Torch_core.Kind.(T Float)) dims
        )

    method log_prob t =
      let lb = Tensor.(type_as (f 0. <= t) t) in
      let ub = Tensor.(type_as (f 1. >= t) t) in
      Tensor.(log (lb * ub))
  end

let dirichlet concentration : Tensor.t t =
  object
    method sample () =
      Tensor.no_grad (fun () ->
          Tensor._sample_dirichlet concentration
        )

    method log_prob t =
      Tensor.(sum (log t * (concentration - f 1.)) + lgamma (sum concentration) - sum (lgamma concentration))
  end

let beta concentration1 concentration0 : Tensor.t t =
  let concentration1 = Tensor.to_type concentration1 ~type_:Torch_core.Kind.(T Float) in
  let concentration0 = Tensor.to_type concentration0 ~type_:Torch_core.Kind.(T Float) in
  let _dirichlet = dirichlet (Tensor.stack [concentration1; concentration0] ~dim:(-1)) in
  object
    method sample () =
      Tensor.no_grad (fun () ->
          Tensor.select (_dirichlet#sample ()) ~dim:(-1) ~index:0
        )

    method log_prob t =
      let v = Tensor.(stack [t; f 1. - t] ~dim:(-1)) in
      _dirichlet#log_prob v
  end
