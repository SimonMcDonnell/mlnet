type t = { data : float array array; shape : int * int }

val show : t -> string
val from_array : float array array -> t
val ones : int -> int -> t
val zeros : int -> int -> t
val sum : t -> t -> t
val sub : t -> t -> t
val matmul : t -> t -> t
val pow : t -> n:float -> t
val mul : t -> const:float -> t
val apply : f:(float -> float) -> t -> t
val transpose : t -> t
val replace : t -> cond:(float -> bool) -> new_val:float -> t
val multiply : t -> t -> t
val divide : t -> t -> t
val sigmoid : t -> t
val he_init : int -> int -> t