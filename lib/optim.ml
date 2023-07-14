open Base


type t = {
  lr : float;
  gamma : float;
  momentum : Matrix.t ref list list;
  step : Nn.model -> unit;
}

let zero_grad layer = 
  layer.Nn.grads <- List.map layer.Nn.grads ~f:(fun grad -> Matrix.mul grad ~const:0.)

let sgd ?(lr=0.003) ?(gamma=0.9) model = 
  let momentum = 
    List.map model.Nn.layers ~f:(fun layer -> 
      List.map layer.Nn.params ~f:(fun mat -> 
        ref @@ Matrix.zeros (fst mat.shape) (snd mat.shape))) 
  in
  let step_layer layer layer_momentum = 
    let layer_momentum' = 
      List.map2_exn layer_momentum layer.Nn.grads ~f:(fun mom grad -> 
        Matrix.sum (Matrix.mul !mom ~const:gamma) (Matrix.mul grad ~const:lr)) 
    in
    let params' = 
      List.map2_exn layer.Nn.params layer_momentum' ~f:(fun param mom -> Matrix.sub param mom)
    in
    List.iter2_exn layer_momentum layer_momentum' ~f:(fun m1 m2 -> m1 := m2);
    layer.params <- params';
    zero_grad layer;
  in
  let step model = List.iter2_exn model.Nn.layers momentum ~f:step_layer
  in
  { lr; gamma; momentum; step }

