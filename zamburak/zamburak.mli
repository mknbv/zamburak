module Utils = Utils

class virtual bandit :
  object
    val mutable total_reward : float

    method virtual narms : int

    method virtual pull : int -> float

    method virtual regret : float

    method reset : unit
  end

class gaussian_bandit :
  float array
  -> float array
  -> object
       inherit bandit

       val max_mean : float

       val mutable npulls : int

       method narms : int

       method pull : int -> float

       method regret : float
     end

class virtual bandit_alg :
  bandit
  -> object
       method narms : int

       method virtual select_arm : int

       method virtual update_stats : int -> float -> unit

       method pull : ?ntimes:int -> unit -> float

       method virtual reset : unit
     end

class ucb :
  bandit
  -> object
       inherit bandit_alg

       val mutable counts : int array

       val mutable means : float array

       val mutable step_count : int

       method regret_bound : ?npulls:int -> float array -> float

       method select_arm : int

       method update_stats : int -> float -> unit

       method reset : unit
     end

class adversarial_bandit :
  (   unit
   -> < narms: int ; select_arm: int ; update_stats: int -> float -> unit ; .. >)
  -> object
       inherit bandit

       val mutable summed_rewards : float array

       method narms : int

       method pull : int -> float

       method regret : float
     end

class exp3 :
  ?horizon:int
  -> ?learning_rate:float
  -> bandit
  -> object
       inherit bandit_alg

       val mutable rewards : float array

       val mutable pulled_arm_prob : float

       method select_arm : int

       method update_stats : int -> float -> unit

       method reset : unit
     end