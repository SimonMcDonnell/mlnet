type t = { data : float array array; shape : int * int; }

val pp : Format.formatter -> t -> unit
val show : t -> string
val from_array : float array array -> t
val ones : int -> int -> t
val zeros : int -> int -> t
val he_init : int -> int -> t
val pow : t -> n:float -> t
val mul : t -> const:float -> t
val sigmoid : t -> t
val apply : t -> f:(float -> float) -> t
val replace : t -> cond:(float -> bool) -> new_val:float -> t
val transpose : t -> t
val sum_rows : t -> t
val sum_cols : t -> t
val sum : t -> t -> t
val sub : t -> t -> t
val matmul : t -> t -> t
val multiply : t -> t -> t
val divide : t -> t -> t
