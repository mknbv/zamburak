class virtual bandit :
  object
    method virtual narms : int
    method virtual pull : int -> float
    method virtual regret : float
  end

class gaussian_bandit :
  float array ->
  float array ->
  object
    val max_mean : float
    val mutable npulls : int
    val mutable total_reward : float
    method narms : int
    method pull : int -> float
    method regret : float
  end

class virtual bandit_alg :
  object
    method virtual select_arm : int
    method virtual update_stats : int -> float -> unit
    method virtual pull : ?ntimes:int -> unit -> float
  end

class ucb :
  bandit ->
  object
    val mutable counts : int array
    val mutable means : float array
    val mutable step_count : int
    method pull : ?ntimes:int -> unit -> float
    method regret_bound : ?npulls:int -> float array -> float
    method select_arm : int
    method update_stats : int -> float -> unit
  end
