(* Code to smaple from normal distribution is based on Box-Muller method: *)
(* https://en.wikipedia.org/wiki/Box–Muller_transform *)
let random_normal () =
  sqrt (-2. *. log (Random.float 1.)) *. cos (2. *. Float.pi *. Random.float 1.)

let argmax nums =
  let rec aux idx idx_max =
    match idx with
    | -1 -> if idx_max >= 0 then idx_max else 0
    | _ -> aux (idx - 1) (if nums.(idx) >= nums.(idx_max) then idx else idx_max)
  in
  let idx = Array.length nums - 1 in
  aux idx idx

module Array = struct
  include Array

  let float_mean arr =
    let rec aux idx mean =
      match idx < Array.length arr with
      | false -> mean
      | true ->
          let count = float idx +. 1. in
          let mean = mean -. (mean /. count) +. (arr.(idx) /. count) in
          aux (idx + 1) mean in
    aux 0 0.

  let float_std ?mean arr =
    let mean = match mean with None -> float_mean arr | Some mean -> mean in
    let rec aux idx std =
      match idx < Array.length arr with
      | false -> std
      | true ->
          let count = float idx +. 1. in
          let std =
            std -. (std /. count) +. (((arr.(idx) -. mean) ** 2.) /. count)
          in
          aux (idx + 1) std in
    let n = float @@ Array.length arr in
    sqrt (n /. (n -. 1.) *. aux 0 0.)

  let accumulate f arr =
    let accum = Array.init (Array.length arr) (fun _ -> arr.(0)) in
    let rec aux idx =
      match idx < Array.length arr with
      | false -> ()
      | true ->
          accum.(idx) <- f accum.(idx - 1) arr.(idx) ;
          aux (idx + 1) in
    aux 1 ; accum
end

let random_categorical probs =
  let csums = Array.accumulate ( +. ) probs in
  if Float.abs (csums.(Array.length csums - 1) -. 1.) > 1e-8 then
    raise (Invalid_argument "probs must sum to 1.") ;
  let sample = Random.float 1. in
  let rec aux idx = if sample <= csums.(idx) then idx else aux (idx + 1) in
  aux 0

let softmax logits =
  let max_logit = Array.fold_left max neg_infinity logits in
  let logits = Array.map (fun lgt -> exp (lgt -. max_logit)) logits in
  let denom = Array.fold_left ( +. ) 0. logits in
  Array.map (fun lgt -> lgt /. denom) logits
