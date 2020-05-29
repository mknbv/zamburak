val random_normal : unit -> float
val argmax : 'a array -> int

module Array : sig
  include module type of Array

  val float_mean : float array -> float
  val float_std : ?mean:float -> float array -> float
  val accumulate : ('a -> 'a -> 'a) -> 'a array -> 'a array
end

val random_categorical : Float.t array -> int
val softmax : Float.t array -> Float.t array
