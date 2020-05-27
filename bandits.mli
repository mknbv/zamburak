class bandit :
  float array ->
  float array ->
  object
    val max_mean : float
    val mutable npulls : int
    val mutable total_reward : float
    method narms : unit -> int
    method pull : int -> float
    method regret : unit -> float
  end

class ucb :
  bandit ->
  object
    val mutable counts : int array
    val mutable means : float array
    val mutable step_count : int
    method pull : ?ntimes:int -> unit -> float
    method regret_bound : ?npulls:int -> float array -> float
    method select_arm : unit -> int
    method update_stats : int -> float -> unit
  end
