type t

val create : float * float -> t
val x : t -> float
val y : t -> float
val to_string : t -> string
val magnitude : t -> float
val distance : t -> t -> float
