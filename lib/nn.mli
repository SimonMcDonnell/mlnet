type layer = {
  name : string;
  mutable params : Matrix.t list;
  mutable grads : Matrix.t list;
  forward : layer -> Matrix.t -> Matrix.t * (Matrix.t -> Matrix.t)
}

type model = {
  layers : layer list;
  forward : Matrix.t -> Matrix.t * (Matrix.t -> unit) 
}

val show : model -> string
val chain : layer list -> model
val linear : int -> int -> layer
val relu : layer
val sigmoid : layer
val mse_loss : Matrix.t -> Matrix.t -> float * Matrix.t
val bce_loss : Matrix.t -> Matrix.t -> float * Matrix.t
val cross_entropy_loss : Matrix.t -> Matrix.t -> float * Matrix.t