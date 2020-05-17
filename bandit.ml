(* Code to smaple from normal distribution is based on Box-Muller method: *)
(* https://en.wikipedia.org/wiki/Box–Muller_transform *)
let random_normal () =
  sqrt (-2. *. log (Random.float 1.))
  *.  cos (2. *. Float.pi *. Random.float 1.)


class bandit means stds =
  let () = assert (Array.length means = Array.length stds) in
  let () = assert (Array.length means > 0) in
  object (self)
    val mutable total_reward = 0.
    val mutable npulls = 0
    val max_mean = Array.fold_left max neg_infinity means

    method narms () = Array.length means
    method pull arm =
      let reward = means.(arm) +. stds.(arm) *. random_normal () in
      total_reward <- total_reward +. reward;
      npulls <- npulls + 1;
      reward
    method regret () = float npulls *. max_mean -. total_reward
  end
