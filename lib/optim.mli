type t = {
  lr : float;
  gamma : float;
  momentum : Matrix.t ref list list;
  step : Nn.model -> unit;
}
val zero_grad : Nn.layer -> unit
val sgd : ?lr:float -> ?gamma:float -> Nn.model -> t
