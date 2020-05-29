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
  let () = assert (Float.equal csums.(Array.length csums - 1) 1.) in
  let sample = Random.float 1. in
  let rec aux idx = if sample <= csums.(idx) then idx else aux (idx + 1) in
  aux 0

let softmax logits =
  let max_logit = Array.fold_left max neg_infinity logits in
  let logits = Array.map (fun lgt -> exp (lgt -. max_logit)) logits in
  let denom = Array.fold_left ( +. ) 0. logits in
  Array.map (fun lgt -> lgt /. denom) logits
