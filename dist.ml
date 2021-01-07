open Core

type 'a t = <
  rsample: (unit -> 'a) option;
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
  let probs = Tensor.to_type probs ~type_:(T Float) in
  let _batch_shape = Tensor.shape probs in
  let _event_shape = [] in
  let lazy logits = lazy (probs_to_logits ~is_binary:true probs) in
  object
    method rsample = None

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
    method rsample = Some (fun () ->
        let shape = Tensor.shape loc in
        let zeros = Tensor.zeros shape in
        let ones = Tensor.ones shape in
        let eps = Tensor.normal2 ~mean:zeros ~std:ones in
        Tensor.( + ) loc  (Tensor.( * ) eps scale)
      )

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
  object (self)
    method rsample = Some (fun () ->
        let value = Tensor.(_standard_gamma concentration / rate) in
        let value_detached = Tensor.detach value in
        Tensor.clamp_min_ value_detached ~min:(Torch.Scalar.float Utils.float_tiny)
      )

    method sample () =
      Tensor.no_grad (fun () ->
          (Option.value_exn self#rsample) ()
        )

    method log_prob t =
      Tensor.(concentration * log rate + (concentration - f 1.) * log t - rate * t - lgamma concentration)
  end

let unif dims : Tensor.t t =
  let _batch_shape = dims in
  let _event_shape = [] in
  object (self)
    method rsample = Some (fun () ->
        Tensor.rand ~kind:(T Float) dims
      )

    method sample () =
      Tensor.no_grad (fun () ->
          (Option.value_exn self#rsample) ()
        )

    method log_prob t =
      let lb = Tensor.(type_as (f 0. <= t) t) in
      let ub = Tensor.(type_as (f 1. >= t) t) in
      Tensor.(log (lb * ub))
  end

let dirichlet concentration : Tensor.t t =
  let concentration = Tensor.to_type concentration ~type_:(T Float) in
  let _batch_shape, _event_shape = List.split_n (Tensor.shape concentration) (List.length (Tensor.shape concentration) - 1) in
  object (self)
    method rsample = Some (fun () ->
        Tensor.dirichlet concentration
      )

    method sample () =
      Tensor.no_grad (fun () ->
          (Option.value_exn self#rsample) ()
        )

    method log_prob t =
      Tensor.(sum1 ~dim:[(-1)] (log t * (concentration - f 1.)) +
              lgamma (sum1 ~dim:[(-1)] concentration) -
              sum1 ~dim:[(-1)] (lgamma concentration))
  end

let beta concentration1 concentration0 : Tensor.t t =
  let concentration1 = Tensor.to_type concentration1 ~type_:(T Float) in
  let concentration0 = Tensor.to_type concentration0 ~type_:(T Float) in
  let _batch_shape = Tensor.shape concentration1 in
  let _event_shape = [] in
  let lazy _dirichlet = lazy (dirichlet (Tensor.stack [concentration1; concentration0] ~dim:(-1))) in
  object (self)
    method rsample = Some (fun () ->
        Tensor.select ((Option.value_exn _dirichlet#rsample) ()) ~dim:(-1) ~index:0
      )

    method sample () =
      Tensor.no_grad (fun () ->
          (Option.value_exn self#rsample) ()
        )

    method log_prob t =
      let v = Tensor.(stack [t; f 1. - t] ~dim:(-1)) in
      _dirichlet#log_prob v
  end

let _clamp_by_zero x =
  Tensor.(clamp_min x ~min:(Torch.Scalar.f 0.) + x - clamp_max x ~max:(Torch.Scalar.f 0.) / f 2.)

let binomial total_count probs : Tensor.t t =
  let total = Tensor.(ones ~scale:(Float.of_int total_count) (shape probs)) in
  let probs = Tensor.to_type probs ~type_:(T Float) in
  let _batch_shape = Tensor.shape probs in
  let _event_shape = [] in
  let lazy logits = lazy (probs_to_logits ~is_binary:true probs) in
  object
    method rsample = None

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
  let probs = Tensor.(probs / sum1 probs ~dim:[(-1)] ~keepdim:true) in
  let _batch_shape = List.drop_last_exn (Tensor.shape probs) in
  let _event_shape = [] in
  let _num_events = List.last_exn (Tensor.shape probs) in
  let lazy logits = lazy (probs_to_logits ~is_binary:false probs) in
  object
    method rsample = None

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
    method rsample = None

    method sample () =
      Tensor.no_grad (fun () ->
          let u = Tensor.(rand (shape probs)) in
          let u = Tensor.clamp_min u ~min:(Torch.Scalar.f Utils.float_tiny) in
          Tensor.(floor (log u / log1p (- probs)))
          |> Tensor.to_type ~type_:(T Int)
        )

    method log_prob t =
      let condition = Tensor.(logical_and (probs = f 1.0) (t = f 0.0)) in
      let probs' = Tensor.where1 ~condition (Tensor.f 0.0) probs in
      Tensor.(t * log1p (- probs') + log probs)
  end

let poisson rate : Tensor.t t =
  let rate = Tensor.to_type rate ~type_:(T Float) in
  let _batch_shape = Tensor.shape rate in
  let _event_shape = [] in
  object
    method rsample = None

    method sample () =
      Tensor.no_grad (fun () ->
          Tensor.poisson rate
          |> Tensor.to_type ~type_:(T Int)
        )

    method log_prob t =
      Tensor.(log rate * t - rate - lgamma (t + f 1.))
  end

let multivariate_normal loc cov_matrix : Tensor.t t =
  let loc = Tensor.to_type loc ~type_:(T Float) in
  let cov_matrix = Tensor.to_type cov_matrix ~type_:(T Float) in
  let _batch_shape, _event_shape = List.split_n (Tensor.shape loc) (List.length (Tensor.shape loc) - 1) in
  let _unbroadcasted_scale_tril = Tensor.cholesky cov_matrix ~upper:false in
  let lazy _invM = lazy Tensor.(inverse (mm _unbroadcasted_scale_tril (tr _unbroadcasted_scale_tril))) in
  object (self)
    method rsample = Some (fun () ->
        let eps = Tensor.(normal2 ~mean:(zeros (shape loc)) ~std:(ones (shape loc))) in
        Tensor.(loc + squeeze1 ~dim:(-1) (mm _unbroadcasted_scale_tril (unsqueeze eps ~dim:(-1))))
      )

    method sample () =
      Tensor.no_grad (fun () ->
          (Option.value_exn self#rsample) ()
        )

    method log_prob t =
      let diff = Tensor.(t - loc) in
      let m = Tensor.(dot (mv _invM ~vec:diff) diff) |> Tensor.float_value in
      let half_log_det = Tensor.(log (diagonal _unbroadcasted_scale_tril ~offset:0 ~dim1:(-2) ~dim2:(-1)) |> sum1 ~dim:[(-1)]) in
      Tensor.(f Float.(-0.5 * (of_int (List.hd_exn _event_shape) * log (2. * pi) + m)) - half_log_det)
  end

let lkj_cholesky dim concentration : Tensor.t t =
  let concentration = Tensor.float_value concentration in
  let _batch_shape = [] in
  let _event_shape = [dim; dim] in
  let lazy _beta = lazy (
    let marginal_conc = Float.(concentration + 0.5 * of_int Int.(dim - 2)) in
    let offset = Tensor.arange ~end_:(Torch.Scalar.i (dim - 1)) ~options:(T Float, Torch.Device.Cpu) in
    let offset = Tensor.cat [Tensor.float_vec [0.]; offset] ~dim:0 in
    let beta_conc1 = Tensor.(offset + f 0.5) in
    let beta_conc0 = Tensor.(unsqueeze ~dim:(-1) (f marginal_conc) - (f 0.5) * offset) in
    beta beta_conc1 beta_conc0
  )
  in
  object
    method rsample = None

    method sample () =
      let y = _beta#sample () in
      let u_normal = Tensor.randn _event_shape |> Tensor.tril ~diagonal:(-1) in
      let u_hypersphere = Tensor.(u_normal / norm2 u_normal ~p:(frobenius_norm u_normal |> float_value |> Torch.Scalar.f) ~dim:[(-1)] ~keepdim:true) in
      let () =
        let first_row = Tensor.get u_hypersphere 0 in
        ignore (Tensor.fill_ first_row ~value:(Torch.Scalar.f 0.) : Tensor.t)
      in
      let w = Tensor.(sqrt y * u_hypersphere) in
      let diag_elems = Tensor.clamp_min ~min:(Torch.Scalar.f Utils.float_tiny) Tensor.(f 1. - sum1 (square w) ~dim:[(-1)])
                       |> Tensor.sqrt
      in
      Tensor.(w + diag_embed diag_elems ~offset:0 ~dim1:(-2) ~dim2:(-1))

    method log_prob t =
      let diag_elems = Tensor.diagonal t ~offset:0 ~dim1:(-1) ~dim2:(-2) |> Tensor.to_list |> List.tl_exn |> Tensor.stack ~dim:0 in
      let order = Tensor.arange1 ~start:(Torch.Scalar.i 2) ~end_:(Torch.Scalar.i (dim + 1)) ~options:(T Float, Torch.Device.Cpu) in
      let order = Tensor.(unsqueeze ~dim:(-1) (f Float.(2. * (concentration - 1.))) + mk_i dim - order) in
      let unnormalized_log_pdf = Tensor.sum1 ~dim:[(-1)] Tensor.(order * log diag_elems) in
      let dm1 = dim - 1 in
      let alpha = Float.(concentration + 0.5 * of_int dm1) in
      let denominator = Tensor.(lgamma (f alpha) * mk_i dm1) in
      let numerator = Tensor.(mvlgamma (f Float.(alpha - 0.5)) ~p:dm1) in
      let pi_constant = Float.(0.5 * of_int dm1 * log pi) in
      let normalize_term = Tensor.(f pi_constant + numerator - denominator) in
      Tensor.(unnormalized_log_pdf - normalize_term)
  end
