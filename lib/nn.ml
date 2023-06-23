open Base


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


let show model = 
  let layer_str = List.map ~f:(fun layer -> layer.name) model.layers |> String.concat ~sep:" -> " in
  let param_str = List.map ~f:(fun layer -> layer.params) model.layers
    |> List.concat
    |> List.map ~f:(fun mat -> Matrix.show mat)
    |> String.concat ~sep:"\n" 
  in
  let grad_str = 
    List.map ~f:(fun layer -> layer.grads) model.layers
    |> List.concat
    |> List.map ~f:(fun mat -> Matrix.show mat)
    |> String.concat ~sep:"\n" 
  in
  "----\nModel:\nlayers: " ^ layer_str ^ "\nparameters:\n" ^ param_str ^ "\ngradients:\n"  ^ 
  grad_str ^ "\n----"


let chain_backprop backprops = 
  let rec ch_b bprops out = 
    match bprops with
    | [] -> ()
    | hd :: tl -> 
      let dX = hd out in
      ch_b tl dX
  in
  ch_b backprops

let chain_forward layers = 
  let rec ch_f (layers: layer list) backprops input = 
    match layers with 
    | [] -> input, chain_backprop backprops
    | hd :: tl ->
      let result, backprop = hd.forward hd input in
      ch_f tl (backprop :: backprops) result
  in
  ch_f layers []

let sequential layers = { layers = layers; forward = chain_forward layers }

let linear n_in n_out = 
  let name = Printf.sprintf "(Linear in=%d out=%d)" n_in n_out in
  let w_init, b_init = Matrix.ones n_in n_out, Matrix.ones 1 n_out in
  let dw_init, db_init = Matrix.zeros n_in n_out, Matrix.zeros 1 n_out in
  
  let forward layer input = 
    let backprop out =
      let dw, db = Matrix.matmul (Matrix.transpose input) out, out in
      let weights = List.nth_exn layer.params 0 in
      let dx = Matrix.matmul out (Matrix.transpose weights) in
      layer.grads <- [ dw; db ];
      dx
    in
    let w, b = List.nth_exn layer.params 0, List.nth_exn layer.params 1 in
    let result = Matrix.matmul input w |> Matrix.sum b in
    result, backprop
  in
  
  { name = name; params = [ w_init; b_init ]; grads = [ dw_init; db_init ]; forward = forward }

