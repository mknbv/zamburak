module Utils = Utils

class type bandit =
  object
    method narms : int

    method pull : int -> float option

    method regret : float

    method reset : unit
  end

class gaussian_bandit :
  float array
  -> ?stds:float array
  -> unit
  -> object
       inherit bandit

       val max_mean : float

       val mutable npulls : int
     end

class type bandit_alg =
  object
    method narms : int

    method pull : ?ntimes:int -> unit -> float

    method reset : unit
  end

class virtual base_bandit_alg :
  bandit
  -> object
       inherit bandit_alg

       method virtual private select_arm : int

       method virtual private update_stats : int -> float -> unit

       method virtual reset : unit
     end

class ucb :
  bandit
  -> object
       inherit base_bandit_alg

       val counts : int array

       val means : float array

       val mutable step_count : int

       method regret_bound : ?npulls:int -> float array -> float

       method private select_arm : int

       method private update_stats : int -> float -> unit

       method reset : unit
     end

class adversarial_bandit :
  (   unit
   -> < narms: int
      ; select_arm: int
      ; update_stats: int -> float -> unit
      ; .. >)
  -> object
       inherit bandit

       val summed_rewards : float array
     end

class random_alg :
  bandit
  -> object
       inherit base_bandit_alg

       method private select_arm : int

       method private update_stats : int -> float -> unit

       method reset : unit
     end

class exp3 :
  ?horizon:int
  -> ?learning_rate:float
  -> bandit
  -> object
       inherit base_bandit_alg

       val rewards : float array

       val mutable selected_arm_prob : float

       method private select_arm : int

       method private update_stats : int -> float -> unit

       method regret_bound : int -> float

       method reset : unit
     end

class exp3ix :
  ?horizon:int
  -> ?learning_rate:float
  -> ?gamma:float
  -> bandit
  -> object
       inherit base_bandit_alg

       val losses : float array

       val mutable selected_arm_prob : float

       method private select_arm : int

       method private update_stats : int -> float -> unit

       method reset : unit
     end
