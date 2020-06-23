open Zamburak
open Matplotlib
open Common

let () =
  let batch_size, npoints, step = (100, 201, 10) in
  let means = [|0.; 0.2; -0.3; 0.1; 0.25|] in
  let stds = Array.make (Array.length means) 1. in
  let make_bandit () = new gaussian_bandit means stds in
  let ucbs = alg_batch batch_size make_bandit (new ucb) in
  let regrets = get_regrets npoints step ucbs in
  let xs = Array.init npoints (fun i -> float step *. float i) in
  Pyplot.semilogy ~color:(Other "gray") ~xs xs ;
  Pyplot.plot_mean_std ~label:"UCB" xs regrets ;
  Pyplot.plot ~xs
    (Array.init (Array.length xs) (fun n ->
         ucbs.(0)#regret_bound ~npulls:(step * n) means)) ;
  set_figure_settings ~xlabel:"$n$" ~ylabel:"Regret@$n$"
    ~labels:[|"Linear regret"; "UCB"; "UCB theoretical bound"|]
    () ;
  Mpl.show ()
