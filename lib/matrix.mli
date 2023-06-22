type t

val show : t -> string
val from_array : float array array -> t
val ones : int -> int -> t
val zeros : int -> int -> t
val sum : t -> t -> t
val sub : t -> t -> t
val matmul : t -> t -> t
val pow : t -> n:float -> t
val mul : t -> const:float -> t
val transpose : t -> t
