open Zamburak
open Matplotlib
open Common

let () =
  let batch_size, npoints, step = (100, 201, 10) in
  let means = [|0.; 0.2; -0.3; 0.1; 0.25|] in
  let ucbs = alg_batch batch_size (new gaussian_bandit means) (new ucb) in
  let regrets = get_regrets npoints step ucbs in
  let xs = Array.map float (Array.range ~step npoints) in
  Pyplot.plot_mean_std ~plot_func:`semilogy ~label:"UCB" xs regrets ;
  Pyplot.plot ~xs
    (Array.map
       (fun n -> ucbs.(0)#regret_bound ~npulls:n means)
       (Array.range ~step npoints)) ;
  Pyplot.plot ~color:(Other "gray") ~xs xs ;
  Pyplot.set_figure_settings ~xlabel:"$n$" ~ylabel:"Regret@$n$"
    ~labels:[|"Linear regret"; "UCB"; "UCB theoretical bound"|]
    () ;
  Mpl.show ()
