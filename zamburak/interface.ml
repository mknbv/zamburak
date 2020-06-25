class type bandit =
  object
    method narms : int

    method pull : int -> float option

    method regret : float

    method reset : unit
  end

class type alg =
  object
    method narms : int

    method pull : ?ntimes:int -> unit -> float

    method regret : float

    method reset : unit
  end
