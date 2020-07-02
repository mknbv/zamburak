open Bandit
open Zamburak.Utils

class stock_ucb (bandit : stock_bandit) =
  object (self)
    inherit Zamburak.ucb (bandit :> Zamburak.bandit)

    method! private update_stats arm reward =
      ignore (arm, reward) ;
      let rewards = bandit#payoffs in
      step_count <- step_count + 1 ;
      let rec update arm =
        match arm < self#narms with
        | false -> ()
        | true ->
            counts.(arm) <- counts.(arm) + 1 ;
            let m, c = (means.(arm), float counts.(arm)) in
            means.(arm) <- means.(arm) -. (m /. c) +. (rewards.(arm) /. c) ;
            update (arm + 1) in
      update 0
  end

let update_vals stale fresh =
  assert (Array.length stale == Array.length fresh) ;
  let rec update idx =
    match idx < Array.length stale with
    | false -> ()
    | true ->
        stale.(idx) <- fresh.(idx) ;
        update (idx + 1) in
  update 0

class stock_exp3 ?horizon ?learning_rate (bandit : stock_bandit) =
  object (self)
    inherit Zamburak.exp3 ?horizon ?learning_rate (bandit :> Zamburak.bandit)

    val probs = Array.make bandit#narms 0.

    method! private select_arm =
      let new_probs =
        rewards |> Array.map (fun rew -> learning_rate *. rew) |> softmax in
      update_vals probs new_probs ;
      random_categorical probs

    method! private update_stats arm reward =
      ignore (arm, reward) ;
      let payoffs = bandit#payoffs in
      let rec update arm =
        match arm < self#narms with
        | false -> ()
        | true ->
            rewards.(arm) <-
              rewards.(arm) +. 1. -. ((1. -. payoffs.(arm)) /. probs.(arm)) ;
            update (arm + 1) in
      update 0
  end
