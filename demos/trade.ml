open Zamburak
open Stock

let run_alg (alg : alg) =
  alg#reset ;
  let _, payoff = alg#pull ~ntimes:Int.max_int () in
  let regret = alg#regret in
  (payoff, regret)

let run_print title alg =
  let payoff, regret = run_alg alg in
  Printf.printf "--- %s ---\n" title ;
  Printf.printf "Payoff: %.4f, Regret: %.4f\n" payoff regret ;
  print_newline ()

let () =
  Random.self_init () ;
  let reader = new csv_reader "stock/data/fortune-500.csv" in
  let stock_bandit = new stock_bandit reader in
  let bandit = (stock_bandit :> bandit) in
  print_newline () ;
  run_print "Random" (new random_alg bandit) ;
  run_print "UCB" (new ucb bandit) ;
  run_print "Stock-UCB" (new stock_ucb stock_bandit) ;
  run_print "Exp3" (new exp3 ~horizon:1000 bandit) ;
  run_print "Stock-Exp3" (new stock_exp3 ~horizon:1000 stock_bandit)
