open Interface
open Utils

class virtual base_bandit_alg (bandit : bandit) =
  object (self : #bandit_alg)
    method narms = bandit#narms

    method virtual private select_arm : int

    method virtual private update_stats : int -> float -> unit

    method pull ?(ntimes = 1) () =
      let rec aux ntimes =
        match ntimes with
        | 0 -> bandit#regret
        | _ -> (
            let arm = self#select_arm in
            match bandit#pull arm with
            | None -> bandit#regret
            | Some reward ->
                self#update_stats arm reward ;
                aux (ntimes - 1) ) in
      aux ntimes

    method reset = bandit#reset
  end

class ucb (bandit : bandit) =
  object (self)
    inherit base_bandit_alg bandit as super

    val mutable step_count = 0

    val means = Array.make bandit#narms 0.

    val counts = Array.make bandit#narms 0

    method private select_arm =
      match step_count < self#narms with
      | true -> step_count
      | false ->
          let t = float step_count +. 1. in
          let time_bonus = 1. +. (t *. (log t ** 2.)) in
          let arm_index arm =
            let m, c = (means.(arm), float counts.(arm)) in
            m +. sqrt (2. *. log time_bonus /. c) in
          Array.init self#narms arm_index |> argmax

    method private update_stats arm reward =
      step_count <- step_count + 1 ;
      counts.(arm) <- counts.(arm) + 1 ;
      let m, c = (means.(arm), float counts.(arm)) in
      means.(arm) <- m -. (m /. c) +. (reward /. c)

    method regret_bound ?(npulls = step_count) means =
      let max_mean = Array.fold_left max neg_infinity means in
      let action_regret mean =
        match mean = max_mean with
        | true -> 0.
        | false ->
            let action_gap = max_mean -. mean in
            action_gap +. (log (float npulls) /. action_gap) in
      means |> Array.map action_regret |> Array.fold_left ( +. ) 0.

    method! reset =
      super#reset ;
      step_count <- 0 ;
      Array.fill means 0 (Array.length means) 0. ;
      Array.fill counts 0 (Array.length counts) 0
  end

class random_alg (bandit : bandit) =
  object
    inherit base_bandit_alg bandit as super

    val probs = Array.init bandit#narms (fun _ -> 1. /. float bandit#narms)

    method private select_arm = probs |> softmax |> random_categorical

    method private update_stats arm reward =
      ignore (arm, reward) ;
      ()

    method! reset = super#reset
  end

class exp3 ?horizon ?learning_rate (bandit : bandit) =
  let get_learning_rate () =
    match (horizon, learning_rate) with
    | None, None | Some _, Some _ ->
        raise
          (Failure "exactly one of horizon and learning_rate must be specified")
    | None, Some learning_rate -> learning_rate
    | Some horizon, None ->
        let narms = float bandit#narms in
        let horizon = float horizon in
        sqrt (2. *. log narms /. (narms *. horizon)) in
  let learning_rate = get_learning_rate () in
  object (self)
    inherit base_bandit_alg bandit as super

    val rewards = Array.make bandit#narms 0.

    val mutable selected_arm_prob = 1. /. float bandit#narms

    method private select_arm =
      let probs =
        rewards |> Array.map (fun rew -> learning_rate *. rew) |> softmax in
      let arm = random_categorical probs in
      selected_arm_prob <- probs.(arm) ;
      arm

    method private update_stats arm reward =
      let selected_arm = arm in
      let rec aux arm =
        match arm with
        | -1 -> ()
        | _ ->
            rewards.(arm) <- rewards.(arm) +. 1. ;
            if arm == selected_arm then
              rewards.(arm) <-
                rewards.(arm) -. ((1. -. reward) /. selected_arm_prob) ;
            aux (arm - 1) in
      aux (Array.length rewards - 1)

    method regret_bound npulls =
      sqrt (2. *. float (npulls * self#narms) *. log (float self#narms))

    method! reset =
      super#reset ;
      Array.fill rewards 0 (Array.length rewards) 0. ;
      selected_arm_prob <- 1. /. float bandit#narms
  end

class exp3ix ?horizon ?learning_rate ?gamma (bandit : bandit) =
  let get_learning_rate () =
    match (horizon, learning_rate) with
    | None, None | Some _, Some _ ->
        raise
          (Failure "exactly one of horizon and learning_rate must be specified")
    | None, Some learning_rate -> learning_rate
    | Some horizon, None ->
        sqrt
          ( 2.
          *. log (float (bandit#narms + 1))
          /. float (horizon * bandit#narms) ) in
  let learning_rate = get_learning_rate () in
  let gamma =
    match gamma with None -> learning_rate /. 2. | Some gamma -> gamma in
  object
    inherit base_bandit_alg bandit as super

    val losses = Array.make bandit#narms 0.

    val mutable selected_arm_prob = 1. /. float bandit#narms

    method private select_arm =
      let probs =
        losses |> Array.map (fun l -> ~-.learning_rate *. l) |> softmax in
      let arm = random_categorical probs in
      selected_arm_prob <- probs.(arm) ;
      arm

    method private update_stats arm reward =
      losses.(arm) <-
        losses.(arm) +. ((1. -. reward) /. (selected_arm_prob +. gamma))

    method! reset =
      super#reset ;
      Array.fill losses 0 (Array.length losses) 0. ;
      selected_arm_prob <- 1. /. float bandit#narms
  end
