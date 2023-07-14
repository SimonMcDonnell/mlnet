open Base


type t = {
  lr : float;
  gamma : float;
  momentum : Matrix.t ref list list;
  step : Nn.layer list -> unit;
}

let zero_grad layers = 
  let zero_layer l = 
    let result = List.map l.Nn.grads ~f:(fun grad -> Matrix.mul grad ~const:0.) in
    l.grads <- result
  in
  List.iter layers ~f:zero_layer

let sgd ?(lr=0.003) ?(gamma=0.9) layers = 
  let momentum = 
    List.map layers ~f:(fun layer -> 
      List.map layer.Nn.params ~f:(fun mat -> 
        ref @@ Matrix.zeros (fst mat.shape) (snd mat.shape))) 
  in
  let l_step layer l_moment = 
    let momentum' = 
      List.map2_exn l_moment layer.Nn.grads ~f:(fun mom grad -> 
        Matrix.sum (Matrix.mul !mom ~const:gamma) (Matrix.mul grad ~const:lr))
    in
    let params' = 
      List.map2_exn layer.Nn.params momentum' ~f:(fun param mom -> Matrix.sub param mom)
    in
    List.iter2_exn l_moment momentum' ~f:(fun m1 m2 -> m1 := m2);
    layer.params <- params';
  in
  let step layers = 
    List.iter2_exn layers momentum ~f:l_step;
    zero_grad layers;
  in
  { lr; gamma; momentum; step }

