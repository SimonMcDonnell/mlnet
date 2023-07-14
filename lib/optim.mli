type t = {
  lr : float;
  gamma : float;
  momentum : Matrix.t ref list list;
  step : Nn.layer list -> unit;
}
val zero_grad : Nn.layer list -> unit
val sgd : ?lr:float -> ?gamma:float -> Nn.layer list -> t
