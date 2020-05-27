open Bandit
open Matplotlib

module Array = struct
  include Array
  let float_mean arr =
    let rec aux idx mean =
      match idx < Array.length arr with
      | false -> mean
      | true ->
          let count = float idx +. 1. in
          let mean = mean -. mean /. count +. arr.(idx) /. count in
          aux (idx + 1) mean
    in aux 0 0.

  let float_std ?mean arr =
    let mean = match mean with
    | None -> float_mean arr
    | Some mean -> mean
    in
    let rec aux idx std =
      match idx < Array.length arr with
      | false -> std
      | true ->
          let count = float idx +. 1. in
          let std = std -. std /. count
                    +. (arr.(idx) -. mean) ** 2. /. count
          in aux (idx + 1) std
    in let n = float @@ Array.length arr in
    sqrt (n /. (n -. 1.) *. (aux 0 0.))
end

let () =
  let means = [|0.; 0.2; -0.3; 0.1; 0.25|] in
  let batch_size, npoints, step = 100, 201, 10 in
  let stds = Array.make (Array.length means) 1. in
  let bandits = Array.init batch_size (fun _ -> new bandit means stds) in
  let ucbs = Array.init batch_size (fun i -> new ucb bandits.(i)) in
  let xs = Array.init npoints (fun i -> float step *. float i) in
  let regrets = Array.init npoints (fun _ ->
    Array.init batch_size (fun i -> ucbs.(i)#pull ~ntimes:step ())
  ) in
  let regret_means = Array.map Array.float_mean regrets in
  let regret_stds = Array.map Array.float_std regrets in

  Pyplot.grid true;
  Pyplot.xlabel "$n$";
  Pyplot.ylabel "Regret@$n$";
  Pyplot.semilogy ~color:(Other "gray") ~xs xs;
  Pyplot.plot ~linewidth:2.~xs regret_means;
  Pyplot.fill_between ~alpha:0.3 ~color:(Other "C0") xs
    (Array.map2 (-.) regret_means regret_stds)
    (Array.map2 (+.) regret_means regret_stds);
  Pyplot.plot ~xs (Array.init (Array.length xs) (fun n ->
      ucbs.(0)#regret_bound ~npulls:(step * n) means));
  Pyplot.legend ~labels:
    [|"Linear regret"; "Regret values"; "Theoretical bound"|] ();
  Mpl.show ()
