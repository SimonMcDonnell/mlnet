type t

val show : t -> string
val create : float array array -> t
val add : t -> t -> t
val sub: t -> t -> t
val dot : t -> t -> t * (t -> t * t)
