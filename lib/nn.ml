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
    | hd :: tl -> ch_b tl (hd out)
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

let chain layers = { layers = layers; forward = chain_forward layers }


(* --------------------------------------------LAYERS-------------------------------------------- *)


let linear n_in n_out = 
  let name = Printf.sprintf "(Linear in=%d out=%d)" n_in n_out in
  let w_init, b_init = Matrix.he_init n_in n_out, Matrix.zeros 1 n_out in
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
  { name; params = [ w_init; b_init ]; grads = []; forward }

let relu = 
  let name = Printf.sprintf "(ReLU)" in
  let forward _ input = 
    let backprop out = 
      let input_ = Matrix.replace input ~cond:(fun x -> Float.(<=) x 0.) ~new_val:0. in
      let input_ = Matrix.replace input_ ~cond:(fun x -> Float.(>) x 0.) ~new_val:1. in
      (Matrix.multiply input_ out)
    in
    let result = Matrix.replace input ~cond:(fun x -> Float.(<) x 0.) ~new_val:0. in
    result, backprop
  in
  { name; params = []; grads = []; forward }

let sigmoid = 
  let name = Printf.sprintf "(Sigmoid)" in
  let forward _ input = 
    let backprop out = 
      let sig_input = Matrix.sigmoid input in
      let nrows, ncols = sig_input.shape in
      let one_minus_sig = Matrix.sub (Matrix.ones nrows ncols) sig_input in
      Matrix.multiply sig_input one_minus_sig |> Matrix.multiply out
    in
    Matrix.sigmoid input, backprop
  in
  { name; params = []; grads = []; forward }


(* ----------------------------------------LOSS FUNCTIONS---------------------------------------- *)


let mean vec = 
  (* mean of a vector of shape (n,1) *)
  let n = vec.Matrix.shape |> fst |> Float.of_int in
  Array.fold ~init:0. ~f:(fun acc x -> acc +. (x.(0) /. n)) vec.Matrix.data

let mse_loss pred target = 
  let n = pred.Matrix.shape |> fst |> Float.of_int in
  let diff = Matrix.sub pred target in
  let grad = Matrix.mul ~const:(2. /. n) diff in
  let result = diff |> Matrix.pow ~n:2. |> mean in
  result, grad

let bce_loss pred target = 
  let one_minus_target = 
    Matrix.sub (Matrix.ones (fst target.Matrix.shape) (snd target.Matrix.shape)) target 
  in
  let one_minus_pred = 
    Matrix.sub (Matrix.ones (fst pred.Matrix.shape) (snd pred.Matrix.shape)) pred 
  in
  let grad = 
    let p1 = Matrix.divide (Matrix.apply ~f:Float.neg target) pred in
    let p2 = Matrix.divide one_minus_target one_minus_pred in
    Matrix.sum p1 p2
  in
  let a =
    Matrix.apply ~f:(fun x -> Float.log x) pred 
    |> Matrix.multiply target 
    |> Matrix.apply ~f:Float.neg
  in
  let b = 
    Matrix.apply ~f:(fun x -> Float.log x) one_minus_pred 
    |> Matrix.multiply one_minus_target
  in
  let result = Matrix.sub a b |> mean in
  result, grad

