type t

val show : t -> string
val shape : 'a array array -> int * int
val create : float array array -> t
val sum : t -> t -> t
val transpose : t -> t
val matmul : t -> t -> t
