type t

val show : t -> string
val from_array : float array array -> t
val ones : int -> int -> t
val zeros : int -> int -> t
val add : t -> t -> t * (t -> t * t)
val sub : t -> t -> t * (t -> t * t)
val dot : t -> t -> t * (t -> t * t)
val pow : t -> n:float -> t
val mul : t -> const:float -> t
val mse : t -> t -> float * t
