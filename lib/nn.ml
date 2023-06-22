open Base


type model = {
  layers : string;
  mutable params : Matrix.t list;
  mutable grads : Matrix.t list;
  forward : model -> Matrix.t -> Matrix.t * (Matrix.t -> Matrix.t list) 
}

let show model = 
  let layer_str = model.layers in
  let param_str = 
    String.concat ~sep:"\n" @@ List.map ~f:(fun mat -> Matrix.show mat) model.params 
  in
  let grad_str = String.concat ~sep:"\n" @@ List.map ~f:(fun mat -> Matrix.show mat) model.grads in
  "----\nModel:\nlayers: " ^ layer_str ^ "\nparameters:\n" ^ param_str ^ "\ngradients:\n"  ^ 
  grad_str ^ "\n----"

let linear n_in n_out = 
  let layers = Printf.sprintf "Linear in=%d out=%d" n_in n_out in
  let w_init, b_init = Matrix.ones n_in n_out, Matrix.ones 1 n_out in
  let dw_init, db_init = Matrix.zeros n_in n_out, Matrix.zeros 1 n_out in
  
  let forward model input = 
    let backprop out =
      let dw, db = List.nth_exn model.grads 0, List.nth_exn model.grads 1 in
      let dw = Matrix.sum dw (Matrix.matmul (Matrix.transpose input) out) in
      let db = Matrix.sum db out in
      model.grads <- [ dw; db ];
      [ dw; db ]
    in
    let w, b = List.nth_exn model.params 0, List.nth_exn model.params 1 in
    let result = Matrix.matmul input w |> Matrix.sum b in
    result, backprop
  in
  
  { layers = layers; params = [ w_init; b_init ]; grads = [ dw_init; db_init ]; forward = forward }

