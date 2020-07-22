open Zamburak
open Stock
open Common

let run_alg batch_size (alg : alg) =
  let rec run idx payoffs regrets =
    match idx < batch_size with
    | false -> (payoffs, regrets)
    | true ->
        alg#reset ;
        let _, payoff = alg#pull ~ntimes:Int.max_int () in
        let regret = alg#regret in
        run (idx + 1) (payoff :: payoffs) (regret :: regrets) in
  run 0 [] []

let run_print ?(batch_size = 1000) title alg =
  let payoffs, regrets = run_alg batch_size alg in
  let payoffs, regrets = (Array.of_list payoffs, Array.of_list regrets) in
  Printf.printf "--- %s ---\n" title ;
  Printf.printf "Payoff: %.4f±%.2f, Regret: %.4f±%.2f\n" (Array.mean payoffs)
    (Array.std payoffs) (Array.mean regrets) (Array.std regrets) ;
  print_newline ()

let () =
  Random.self_init () ;
  ["data/fortune-500.csv"; "data/random-stocks.csv"]
  |> List.iter (fun filename ->
         let reader = new csv_reader filename in
         let stock_bandit = new stock_bandit reader in
         let bandit = (stock_bandit :> bandit) in
         Printf.printf "\n**** file: '%s' ****\n\n" filename ;
         run_print "Random" (new random_alg bandit) ;
         run_print "UCB" (new ucb bandit) ;
         run_print "Stock-UCB" (new stock_ucb stock_bandit) ;
         run_print "Exp3" (new exp3 ~horizon:1000 bandit) ;
         run_print "Stock-Exp3" (new stock_exp3 ~horizon:1000 stock_bandit))
