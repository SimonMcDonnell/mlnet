open Base

type t = { data : float array array; shape : int * int } [@@deriving show]
type op = Add | Sub

let shape data = (Array.length data, Array.length data.(0))

let create data = { data; shape = shape data }

let apply_elem ~f data = Array.map ~f:(fun row -> Array.map ~f row) data

let pow mat ~n = 
  let result = apply_elem ~f:(fun x -> x **. n) mat.data in
  { data = result; shape = mat.shape }

let mul mat ~const = 
  let result = apply_elem ~f:(fun x -> x *. const) mat.data in
  { data = result; shape = mat.shape }

let matrix_op ~op d1 d2 = 
  (* add or subtract matrices *)
  let op_fn ~op d1 d2 = 
    match op with
    | Add -> Array.map2_exn ~f:(fun x y -> x +. y) d1 d2
    | Sub -> Array.map2_exn ~f:(fun x y -> x -. y) d1 d2
  in
  Array.map2_exn ~f:(op_fn ~op) d1 d2

let repeat_elem ~n elem = 
  match elem with 
  | [| x |] -> Array.init n ~f:(fun _ -> x) 
  | lst -> lst

let sum m1 m2 =
  (* if not same shape, check for array broadcast and alter shape to make addition work *)
  if snd m1.shape = 1 then
    let m1_reshaped = Array.map ~f:(repeat_elem ~n:(snd m2.shape)) m1.data in
    let result = matrix_op ~op:Add m1_reshaped m2.data in
    { data = result; shape = m2.shape }
  else if snd m2.shape = 1 then
    let m2_reshaped = Array.map ~f:(repeat_elem ~n:(snd m1.shape)) m2.data in
    let result = matrix_op ~op:Add m1.data m2_reshaped in
    { data = result; shape = m1.shape }
  else 
    let result = matrix_op ~op:Add m1.data m2.data in
    { data = result; shape = m1.shape }

let subtract m1 m2 =
  (* if not same shape, check for array broadcast and alter shape to make subtraction work *)
    if snd m1.shape = 1 then
      let m1_reshaped = Array.map ~f:(repeat_elem ~n:(snd m2.shape)) m1.data in
      let result = matrix_op ~op:Sub m1_reshaped m2.data in
      { data = result; shape = m2.shape }
    else if snd m2.shape = 1 then
      let m2_reshaped = Array.map ~f:(repeat_elem ~n:(snd m1.shape)) m2.data in
      let result = matrix_op ~op:Sub m1.data m2_reshaped in
      { data = result; shape = m1.shape }
    else 
      let result = matrix_op ~op:Sub m1.data m2.data in
      { data = result; shape = m1.shape }

let transpose data =
  let num_rows, num_cols = shape data in
  Array.init num_cols ~f:(fun i ->
    Array.init num_rows ~f:(fun j -> data.(j).(i)))

let matmul d1 d2 =
  let d2_T = transpose d2 in
  let product v1 v2 = Array.fold2_exn ~init:0. ~f:(fun acc a b -> acc +. (a *. b)) v1 v2 in
  Array.map ~f:(fun row -> Array.map ~f:(product row) d2_T) d1


(* ------------------------------- Differentiable Functions ------------------------------- *)


let add a b = 
  let grad (out: t) = out, out in
  let result = sum a b in
  result, grad

let sub a b = 
  let grad (out: t) = out, out in
  let result = subtract a b in
  result, grad

let dot a b =
  let grad out =
    let da = matmul out.data (transpose b.data) in
    let db = matmul (transpose a.data) out.data in
    let da_shape, db_shape = ((fst out.shape, fst b.shape), (snd a.shape, snd out.shape)) in
    { data = da; shape = da_shape }, { data = db; shape = db_shape }
  in
  let result = matmul a.data b.data in
  { data = result; shape = (fst a.shape, snd b.shape) }, grad


(* ------------------------------- Error Functions ------------------------------- *)

let mean mat = Array.fold ~init:0. ~f:(fun acc x -> acc +. x.(0)) mat.data

let mse pred labels = 
  let n = pred.shape |> fst |> Float.of_int in
  let diff = { data = matrix_op ~op:Sub pred.data labels.data; shape = pred.shape } in
  let grad = mul ~const:(2. /. n) diff in
  let result = diff |> pow ~n:2. |> mean in
  result, grad

