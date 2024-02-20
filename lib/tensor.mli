type t
val from_array : float array array -> t
val shape : t -> int * int
val get : t -> int -> int -> float
val print : t -> unit
val show : t -> string
val ones : int -> int -> t
val zeros : int -> int -> t
val he_init : int -> int -> t
val scalar_pow : n:float -> t -> t
val scalar_mul : n:float -> t -> t
val scalar_add : n:float -> t -> t
val scalar_sub : n:float -> t -> t
val sigmoid : t -> t
val softmax : ?axis:int -> t -> t
val map : f:(float -> float) -> t -> t
val mapi : f:(int -> float -> float) -> t -> t
val map2 : f:(float -> float -> float) -> t -> t -> t
val neg : t -> t
val exp : t -> t
val log : t -> t
val transpose : t -> t
val sum_rows : t -> t
val sum_cols : t -> t
val mean : t -> float
val add : t -> t -> t
val sub : t -> t -> t
val dot : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t