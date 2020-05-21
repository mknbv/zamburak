open Bandit


let argmax nums =
  let rec aux idx idx_max =
    match idx with
    | -1 -> if idx_max >= 0 then Some idx_max else None
    | _ -> aux (idx - 1) (if nums.(idx) >= nums.(idx_max)
                          then idx else idx_max)
  in let idx = Array.length nums - 1
  in aux idx idx


class ucb (bandit : bandit) =
  let narms = bandit#narms () in
  object (self)
    val mutable step_count = 0
    val mutable means = Array.make narms 0.
    val mutable counts = Array.make narms 0

  method select_arm () =
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

  method pull () =
    let arm = self#select_arm () in
    let reward = bandit#pull arm in
    self#update_stats arm reward;
    bandit#regret ()

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
