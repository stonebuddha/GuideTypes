type 'a t = <
  sample: unit -> 'a * float;
  log_prob: 'a -> float;
>
