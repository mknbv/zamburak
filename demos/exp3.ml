open Zamburak
open Matplotlib

let () =
  let batch_size, npoints, step = (100, 201, 10) in
  let bandits =
    Array.init batch_size (fun _ ->
        new adversarial_bandit (fun () ->
            new ucb (new gaussian_bandit (Array.make 2 0.) (Array.make 2 1.))))
  in
  let ucbs = Array.init batch_size (fun i -> new ucb bandits.(i)) in
  let ucb_regrets =
    Array.init npoints (fun _ ->
        Array.init batch_size (fun i -> ucbs.(i)#pull ~ntimes:step ())) in
  let ucb_means = Array.map Utils.Array.float_mean ucb_regrets in
  let ucb_stds = Array.map Utils.Array.float_std ucb_regrets in
  Array.iter (fun bandit -> bandit#reset) bandits ;
  let exp3s =
    Array.init batch_size (fun i ->
        new exp3 ~horizon:(npoints * step) bandits.(i)) in
  let exp3_regrets =
    Array.init npoints (fun _ ->
        Array.init batch_size (fun i -> exp3s.(i)#pull ~ntimes:step ())) in
  let exp3_means = Array.map Utils.Array.float_mean exp3_regrets in
  let exp3_stds = Array.map Utils.Array.float_std exp3_regrets in
  Array.iter (fun bandit -> bandit#reset) bandits ;
  let exp3ixs =
    Array.init batch_size (fun i ->
        new exp3ix ~horizon:(npoints * step) bandits.(i)) in
  let exp3ix_regrets =
    Array.init npoints (fun _ ->
        Array.init batch_size (fun i -> exp3ixs.(i)#pull ~ntimes:step ()))
  in
  let exp3ix_means = Array.map Utils.Array.float_mean exp3ix_regrets in
  let exp3ix_stds = Array.map Utils.Array.float_std exp3ix_regrets in
  let xs = Array.init npoints (fun i -> float (i * step)) in
  Pyplot.semilogy ~color:Black ~xs xs ;
  Pyplot.semilogy ~xs ucb_means ;
  Pyplot.fill_between ~alpha:0.2 xs
    (Array.map2 ( -. ) ucb_means ucb_stds)
    (Array.map2 ( +. ) ucb_means ucb_stds) ;
  Pyplot.semilogy ~color:(Other "gray") ~xs
    (Array.init npoints (fun n -> exp3s.(0)#regret_bound (n * step))) ;
  Pyplot.semilogy ~xs exp3_means ;
  Pyplot.fill_between ~alpha:0.2 xs
    (Array.map2 ( -. ) exp3_means exp3_stds)
    (Array.map2 ( +. ) exp3_means exp3_stds) ;
  Pyplot.semilogy ~xs exp3ix_means ;
  Pyplot.fill_between ~alpha:0.2 xs
    (Array.map2 ( -. ) exp3ix_means exp3ix_stds)
    (Array.map2 ( +. ) exp3ix_means exp3ix_stds) ;
  Pyplot.legend
    ~labels:[|"Linear"; "UCB"; "Exp3"; "Exp3 theoretical bound"; "Exp3-IX"|]
    () ;
  Pyplot.grid true ;
  Mpl.show ()
