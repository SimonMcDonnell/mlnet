type layer
type model = {
  layers : layer list;
  forward : Matrix.t -> Matrix.t * (Matrix.t -> unit) 
}

val show : model -> string
val sequential : layer list -> model
val linear : int -> int -> layer
val relu : layer
val sigmoid : layer
val mse_loss : Matrix.t -> Matrix.t -> float * Matrix.t
val bce_loss : Matrix.t -> Matrix.t -> float * Matrix.t