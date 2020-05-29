val random_normal : unit -> float
val argmax : 'a array -> int option

module Array : sig
  include module type of Array

  val accumulate : ('a -> 'a -> 'a) -> 'a array -> 'a array
end

val random_categorical : Float.t array -> int
val softmax : Float.t array -> Float.t array
