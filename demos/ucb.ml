open Zamburak
open Matplotlib

let () =
  let means = [|0.; 0.2; -0.3; 0.1; 0.25|] in
  let batch_size, npoints, step = (100, 201, 10) in
  let stds = Array.make (Array.length means) 1. in
  let bandits =
    Array.init batch_size (fun _ -> new gaussian_bandit means stds) in
  let ucbs = Array.init batch_size (fun i -> new ucb bandits.(i)) in
  let xs = Array.init npoints (fun i -> float step *. float i) in
  let regrets =
    Array.init npoints (fun _ ->
        Array.init batch_size (fun i -> ucbs.(i)#pull ~ntimes:step ())) in
  let regret_means = Array.map Utils.Array.float_mean regrets in
  let regret_stds = Array.map Utils.Array.float_std regrets in
  Pyplot.grid true ;
  Pyplot.xlabel "$n$" ;
  Pyplot.ylabel "Regret@$n$" ;
  Pyplot.semilogy ~color:(Other "gray") ~xs xs ;
  Pyplot.plot ~linewidth:2. ~xs regret_means ;
  Pyplot.fill_between ~alpha:0.3 ~color:(Other "C0") xs
    (Array.map2 ( -. ) regret_means regret_stds)
    (Array.map2 ( +. ) regret_means regret_stds) ;
  Pyplot.plot ~xs
    (Array.init (Array.length xs) (fun n ->
         ucbs.(0)#regret_bound ~npulls:(step * n) means)) ;
  Pyplot.legend
    ~labels:[|"Linear regret"; "Regret values"; "Theoretical bound"|]
    () ;
  Mpl.show ()
