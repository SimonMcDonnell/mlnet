val fit :
  Nn.model ->
  Matrix.t ->
  Matrix.t ->
  epochs:int ->
  loss_fn:(Matrix.t -> Matrix.t -> float * Matrix.t) -> optimizer:Optim.t -> unit
