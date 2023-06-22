type model = {
  layers : string;
  mutable params : Matrix.t list;
  mutable grads : Matrix.t list;
  forward : model -> Matrix.t -> Matrix.t * (Matrix.t -> Matrix.t list) 
}

val show : model -> string
val linear : int -> int -> model
