open Alg
open Interface
open Utils

class gaussian_bandit means ?stds () =
  let stds =
    match stds with
    | None -> Array.make (Array.length means) 1.
    | Some stds -> stds in
  let () = assert (Array.length means = Array.length stds) in
  let () = assert (Array.length means > 0) in
  object (_ : #bandit)
    val mutable total_reward = 0.

    val mutable npulls = 0

    val max_mean = Array.fold_left max neg_infinity means

    method narms = Array.length means

    method pull arm =
      let reward = means.(arm) +. (stds.(arm) *. random_normal ()) in
      total_reward <- total_reward +. reward ;
      npulls <- npulls + 1 ;
      Some reward

    method regret = (float npulls *. max_mean) -. total_reward

    method reset =
      total_reward <- 0. ;
      npulls <- 0
  end

class ucb_adversarial_bandit narms =
  object (_ : #bandit)
    val mutable total_reward = 0.

    val alg =
      object
        inherit ucb (new gaussian_bandit (Array.make narms 0.) ()) as self

        method! select_arm = self#select_arm

        method! update_stats = self#update_stats
      end

    val summed_rewards = Array.make narms 0.

    method narms = alg#narms

    method pull arm =
      let worst_arm = alg#select_arm in
      let rewards =
        Array.init alg#narms (fun arm -> Bool.to_float (arm <> worst_arm))
      in
      alg#update_stats worst_arm rewards.(worst_arm) ;
      let rec update_summed_rewards idx =
        match idx < Array.length summed_rewards with
        | false -> ()
        | true ->
            summed_rewards.(idx) <- summed_rewards.(idx) +. rewards.(idx) ;
            update_summed_rewards (idx + 1) in
      update_summed_rewards 0 ;
      total_reward <- total_reward +. rewards.(arm) ;
      Some rewards.(arm)

    method regret =
      Array.fold_left max neg_infinity summed_rewards -. total_reward

    method reset =
      total_reward <- 0. ;
      Array.fill summed_rewards 0 (Array.length summed_rewards) 0.
  end
