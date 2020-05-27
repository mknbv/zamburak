(* Code to smaple from normal distribution is based on Box-Muller method: *)
(* https://en.wikipedia.org/wiki/Box–Muller_transform *)
let random_normal () =
  sqrt (-2. *. log (Random.float 1.))
  *.  cos (2. *. Float.pi *. Random.float 1.)


class virtual bandit =
  object
    method virtual narms : int
    method virtual pull : int -> float
    method virtual regret : float
  end

class gaussian_bandit means stds =
  let () = assert (Array.length means = Array.length stds) in
  let () = assert (Array.length means > 0) in
  object
    inherit bandit
    val mutable total_reward = 0.
    val mutable npulls = 0
    val max_mean = Array.fold_left max neg_infinity means

    method narms = Array.length means
    method pull arm =
      let reward = means.(arm) +. stds.(arm) *. random_normal () in
      total_reward <- total_reward +. reward;
      npulls <- npulls + 1;
      reward
    method regret = float npulls *. max_mean -. total_reward
  end


let argmax nums =
  let rec aux idx idx_max =
    match idx with
    | -1 -> if idx_max >= 0 then Some idx_max else None
    | _ -> aux (idx - 1) (if nums.(idx) >= nums.(idx_max)
                          then idx else idx_max)
  in let idx = Array.length nums - 1
  in aux idx idx


class virtual bandit_alg =
  object
    method virtual select_arm : int
    method virtual update_stats : int -> float -> unit
    method virtual pull : ?ntimes:int -> unit -> float
  end

class ucb (bandit : bandit) =
  let narms = bandit#narms in
  object (self)
    inherit bandit_alg
    val mutable step_count = 0
    val mutable means = Array.make narms 0.
    val mutable counts = Array.make narms 0

  method select_arm =
    match step_count < narms with
    | true -> step_count
    | false ->
      let t = float step_count +. 1. in
      let time_bonus = 1. +. t *. (log t) ** 2. in
      let arm_index arm =
        let m, c = means.(arm), float counts.(arm) in
          m +. sqrt (2. *. log time_bonus /. c) in
      match Array.init narms arm_index |> argmax with
      | None -> -1  (* Shouldn't happen since narms > 0 and argmax code *)
      | Some arm  -> arm

  method update_stats arm reward =
    step_count <- step_count + 1;
    counts.(arm) <- counts.(arm) + 1;
    let m, c = means.(arm), float counts.(arm) in
    means.(arm) <- m -. m /. c +. reward /. c;

  method pull ?(ntimes=1) () =
    let rec aux ntimes =
      match ntimes with
      | 0 -> bandit#regret
      | _ ->
        let arm = self#select_arm in
        let reward = bandit#pull arm in
        self#update_stats arm reward;
        aux (ntimes - 1)
    in aux ntimes

  method regret_bound ?(npulls=step_count) means =
    let max_mean = Array.fold_left max neg_infinity means in
    let action_regret mean =
      match mean = max_mean with
      | true -> 0.
      | false ->
      let action_gap = max_mean -. mean in
      action_gap +. log (float npulls) /. action_gap
    in means |> Array.map action_regret |> Array.fold_left (+.) 0.

  end
