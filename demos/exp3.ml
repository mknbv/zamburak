open Zamburak
open Matplotlib
open Common

let () =
  let batch_size, npoints, step, narms = (100, 201, 10, 2) in
  let alg_batch =
    alg_batch batch_size (fun () -> new ucb_adversarial_bandit narms) in
  let get_regrets = get_regrets npoints step in
  let exp3 = new exp3 ~horizon:(npoints * step) in
  let exp3ix = new exp3ix ~horizon:(npoints * step) in
  let xs = Array.map float (Array.range ~step npoints) in
  Pyplot.plot_mean_std ~plot_func:`semilogy xs
    (get_regrets (alg_batch (new ucb))) ;
  Pyplot.plot_mean_std xs (get_regrets (alg_batch exp3)) ;
  Pyplot.plot_mean_std xs (get_regrets (alg_batch exp3ix)) ;
  Pyplot.semilogy ~color:(Other "gray") ~xs
    (Array.map (fun n -> exp3_regret_bound 2 n) (Array.range ~step npoints)) ;
  Pyplot.semilogy ~color:Black ~xs xs ;
  Pyplot.set_figure_settings ~xlabel:"$n$" ~ylabel:"Regret@$n$"
    ~labels:[|"UCB"; "Exp3"; "Exp3-IX"; "Exp3 theoretical bound"; "Linear"|]
    () ;
  Mpl.show ()
