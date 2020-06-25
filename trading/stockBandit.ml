class file_reader fname =
  object
    val mutable ic = open_in fname

    method next_line =
      try Some (input_line ic) with
      | End_of_file -> close_in ic ; None
      | e -> close_in_noerr ic ; raise e

    method reset =
      close_in ic ;
      ic <- open_in fname
  end

class csv_reader ?(sep = ',') fname =
  object (self)
    inherit file_reader fname as super

    method line_to_data line = String.split_on_char sep line

    method next_data =
      match super#next_line with
      | None -> None
      | Some line -> Some (self#line_to_data line)
  end

let get_tickers header =
  let rec aux header idx tickers =
    match (header, idx mod 2) with
    | [], _ -> tickers
    | s :: header, 1 ->
        let new_ticker = String.split_on_char '-' s |> List.hd in
        aux header (idx + 1) (new_ticker :: tickers)
    | _ :: header, _ -> aux header (idx + 1) tickers in
  aux header 0 [] |> List.rev

class stock_bandit ?(investment = 1.) data_reader =
  let header =
    match data_reader#next_data with
    | None -> raise (Failure "data_reader cannot be empty")
    | Some line -> line in
  let tickers = Array.of_list @@ get_tickers header in
  let narms = Array.length tickers in
  object (self : #Zamburak.bandit)
    val mutable total_reward = 0.

    val summed_rewards = Array.make narms 0.

    method narms = narms

    method private payoffs data =
      let payoff ticket_index =
        let shares_bought =
          investment /. float_of_string data.((2 * ticket_index) + 1) in
        (shares_bought *. float_of_string data.((2 * ticket_index) + 2))
        -. investment in
      Array.init narms payoff

    method pull arm =
      match data_reader#next_data with
      | None -> None
      | Some data ->
          let payoffs = data |> Array.of_list |> self#payoffs in
          let rec update_summed_rewards idx =
            match idx < Array.length summed_rewards with
            | false -> ()
            | true ->
                summed_rewards.(idx) <- summed_rewards.(idx) +. payoffs.(idx) ;
                update_summed_rewards (idx + 1) in
          update_summed_rewards 0 ;
          let reward = payoffs.(arm) in
          total_reward <- total_reward +. reward ;
          Some reward

    method regret =
      Array.fold_left max neg_infinity summed_rewards -. total_reward

    method reset =
      total_reward <- 0. ;
      Array.fill summed_rewards 0 (Array.length summed_rewards) 0. ;
      data_reader#reset ;
      ignore data_reader#next_data
  end
