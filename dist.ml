open Core

type 'a t = <
  sample: unit -> 'a;
  log_prob: 'a -> Tensor.t;
>

let clamp_probs probs =
  Tensor.clamp probs ~min:(Torch.Scalar.float Utils.float_eps) ~max:(Torch.Scalar.float Float.(1. - Utils.float_eps))

let probs_to_logits ~is_binary probs =
  let ps_clamped = clamp_probs probs in
  if is_binary then
    Tensor.(log ps_clamped - log1p (- ps_clamped))
  else
    Tensor.log ps_clamped

let bernoulli probs : Tensor.t t =
  let probs = Tensor.(to_type probs ~type_:(T Float)) in
  let _batch_shape = Tensor.shape probs in
  let _event_shape = [] in
  let lazy logits = lazy (probs_to_logits ~is_binary:true probs) in
  object
    method sample () =
      Tensor.no_grad (fun () ->
          let t = Tensor.bernoulli probs in
          Tensor.to_type t ~type_:(T Bool)
        )

    method log_prob t =
      Tensor.(- (binary_cross_entropy_with_logits logits ~target:t ~weight:None ~pos_weight:None ~reduction:Torch_core.Reduction.None))
  end

let normal loc scale : Tensor.t t =
  let loc = Tensor.to_type loc ~type_:(T Float) in
  let scale = Tensor.to_type scale ~type_:(T Float) in
  let _batch_shape = Tensor.shape loc in
  let _event_shape = [] in
  let lazy variance_dbl = lazy (Tensor.scale (Tensor.square scale) 2.) in
  let lazy log_scale = lazy (Tensor.log scale) in
  let lazy shift = lazy (Tensor.f Float.(log (sqrt (2. * pi)))) in
  object
    method sample () =
      Tensor.no_grad (fun () ->
          Tensor.normal2 ~mean:loc ~std:scale
        )

    method log_prob t =
      Tensor.((- (square (t - loc))) / variance_dbl - log_scale - shift)
  end

let gamma concentration rate : Tensor.t t =
  let concentration = Tensor.to_type concentration ~type_:(T Float) in
  let rate = Tensor.to_type rate ~type_:(T Float) in
  let _batch_shape = Tensor.shape concentration in
  let _event_shape = [] in
  object
    method sample () =
      Tensor.no_grad (fun () ->
          let value = Tensor.(_standard_gamma concentration / rate) in
          let value_detached = Tensor.detach value in
          Tensor.clamp_min_ value_detached ~min:(Torch.Scalar.float Utils.float_tiny)
        )

    method log_prob t =
      Tensor.(concentration * log rate + (concentration - f 1.) * log t - rate * t - lgamma concentration)
  end

let unif dims : Tensor.t t =
  let _batch_shape = dims in
  let _event_shape = [] in
  object
    method sample () =
      Tensor.no_grad (fun () ->
          Tensor.rand ~kind:(T Float) dims
        )

    method log_prob t =
      let lb = Tensor.(type_as (f 0. <= t) t) in
      let ub = Tensor.(type_as (f 1. >= t) t) in
      Tensor.(log (lb * ub))
  end

let dirichlet concentration : Tensor.t t =
  let concentration = Tensor.to_type concentration ~type_:(T Float) in
  let _batch_shape, _event_shape = List.split_n (Tensor.shape concentration) (List.length (Tensor.shape concentration) - 1) in
  object
    method sample () =
      Tensor.no_grad (fun () ->
          Tensor._sample_dirichlet concentration
        )

    method log_prob t =
      Tensor.(sum1 ~dim:[(-1)] ~keepdim:false ~dtype:(T Float) (log t * (concentration - f 1.)) +
              lgamma (sum1 ~dim:[(-1)] ~keepdim:false ~dtype:(T Float) concentration) -
              sum1 ~dim:[(-1)] ~keepdim:false ~dtype:(T Float) (lgamma concentration))
  end

let beta concentration1 concentration0 : Tensor.t t =
  let concentration1 = Tensor.to_type concentration1 ~type_:(T Float) in
  let concentration0 = Tensor.to_type concentration0 ~type_:(T Float) in
  let _batch_shape = Tensor.shape concentration1 in
  let _event_shape = [] in
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

let _clamp_by_zero x =
  Tensor.(clamp_min x ~min:(Torch.Scalar.f 0.) + x - clamp_max x ~max:(Torch.Scalar.f 0.) / f 2.)

let binomial total_count probs : Tensor.t t =
  let total = Tensor.(ones (shape probs) * (f (Float.of_int total_count))) in
  let probs = Tensor.to_type probs ~type_:(T Float) in
  let _batch_shape = Tensor.shape probs in
  let _event_shape = [] in
  let lazy logits = lazy (probs_to_logits ~is_binary:true probs) in
  object
    method sample () =
      Tensor.no_grad (fun () ->
          Tensor.binomial ~count:total ~prob:probs
          |> Tensor.to_type ~type_:(T Int)
        )

    method log_prob t =
      let log_factorial_n = Tensor.(lgamma (total + f 1.0)) in
      let log_factorial_k = Tensor.(lgamma (t + f 1.0)) in
      let log_factorial_nmk = Tensor.(lgamma (total - t + f 1.0)) in
      let normalize_term = Tensor.(
          total * (_clamp_by_zero logits) +
          total * (log1p (exp (- (abs logits)))) -
          log_factorial_n
        )
      in
      Tensor.(t * logits - log_factorial_k - log_factorial_nmk - normalize_term)
  end

let categorical probs : Tensor.t t =
  let probs = Tensor.to_type probs ~type_:(T Float) in
  let probs = Tensor.(probs / sum1 probs ~dim:[(-1)] ~keepdim:true ~dtype:(T Float)) in
  let _batch_shape = List.drop_last_exn (Tensor.shape probs) in
  let _event_shape = [] in
  let _num_events = List.last_exn (Tensor.shape probs) in
  let lazy logits = lazy (probs_to_logits ~is_binary:false probs) in
  object
    method sample () =
      let probs_2d = Tensor.reshape probs ~shape:[(-1); _num_events] in
      let samples_2d = Tensor.tr (Tensor.multinomial probs_2d ~num_samples:1 ~replacement:true) in
      Tensor.reshape samples_2d ~shape:_batch_shape
      |> Tensor.to_type ~type_:(T Int)

    method log_prob t =
      let t_un = Tensor.unsqueeze t ~dim:(-1) in
      Tensor.gather logits ~dim:(-1) ~index:t_un ~sparse_grad:false
      |> Tensor.squeeze1 ~dim:(-1)
  end

let geometric probs : Tensor.t t =
  let probs = Tensor.to_type probs ~type_:(T Float) in
  let _batch_shape = Tensor.shape probs in
  let _event_shape = [] in
  object
    method sample () =
      Tensor.no_grad (fun () ->
          let u = Tensor.(rand (shape probs)) in
          let u = Tensor.clamp_min u ~min:(Torch.Scalar.f Utils.float_tiny) in
          Tensor.(floor (log u / log1p (- probs)))
          |> Tensor.to_type ~type_:(T Int)
        )

    method log_prob t =
      let condition = Tensor.(logical_not (logical_and (probs = f 1.0) (t = f 0.0))) in
      let probs' = Tensor._s_where ~condition probs (Tensor.f 0.0) in
      Tensor.(t * log1p (- probs') + log probs)
  end

let poisson rate : Tensor.t t =
  let rate = Tensor.to_type rate ~type_:(T Float) in
  let _batch_shape = Tensor.shape rate in
  let _event_shape = [] in
  object
    method sample () =
      Tensor.no_grad (fun () ->
          Tensor.poisson rate
        )

    method log_prob t =
      Tensor.(log rate * t - rate - lgamma (t + f 1.))
  end

let multivariate_normal loc cov_matrix =
  let loc = Tensor.to_type loc ~type_:(T Float) in
  let cov_matrix = Tensor.to_type cov_matrix ~type_:(T Float) in
  let _batch_shape, _event_shape = List.split_n (Tensor.shape loc) (List.length (Tensor.shape loc) - 1) in
  let _unbroadcasted_scale_tril = Tensor.cholesky cov_matrix ~upper:false in
  let _M = Tensor.mm _unbroadcasted_scale_tril (Tensor.tr _unbroadcasted_scale_tril) in
  let _Minv = Tensor.inverse _M in
  let event_shape = Tensor.shape cov_matrix |> List.last_exn in
  object
    method sample () =
      Tensor.no_grad (fun () ->
          let eps = Tensor.(normal2 ~mean:(zeros (shape loc)) ~std:(ones (shape loc))) in
          Tensor.(loc + squeeze1 ~dim:(-1) (mm _unbroadcasted_scale_tril (unsqueeze eps ~dim:(-1))))
        )

    method log_prob t =
      let diff = Tensor.(t - loc) in
      let m = Tensor.(dot (mv _Minv ~vec:diff) diff) |> Tensor.float_value in
      let half_log_det = Tensor.(log (diagonal _unbroadcasted_scale_tril ~offset:0 ~dim1:(-2) ~dim2:(-1)) |> sum1 ~dim:[(-1)] ~keepdim:false ~dtype:(T Float)) in
      Tensor.(f Float.(-0.5 * (of_int event_shape * log (2. * pi) + m)) - half_log_det)
  end
