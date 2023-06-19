open Base

type t = { data : float array array; shape : int * int } [@@deriving show]
type op = Add | Sub

let shape data = (Array.length data, Array.length data.(0))

let create data = { data; shape = shape data }

let apply_elem ~f mat = 
  let result = Array.map ~f:(fun row -> Array.map ~f row) mat.data in
  { data = result; shape = shape result}

let pow mat ~n = apply_elem ~f:(fun x -> x **. n) mat

let mul mat ~const = apply_elem ~f:(fun x -> x *. const) mat

let matrix_op ~op d1 d2 = 
  (* add or subtract matrices *)
  let op_fn ~op d1 d2 = 
    match op with
    | Add -> Array.map2_exn ~f:(fun x y -> x +. y) d1 d2
    | Sub -> Array.map2_exn ~f:(fun x y -> x -. y) d1 d2
  in
  let op_result = Array.map2_exn ~f:(op_fn ~op) d1 d2 in
  { data = op_result; shape = shape op_result}

let repeat_elem ~n elem = 
  match elem with 
  | [| x |] -> Array.init n ~f:(fun _ -> x) 
  | lst -> lst

let sum m1 m2 =
  (* if not same shape, check for array broadcast and alter shape to make addition work *)
  if snd m1.shape = 1 then
    let m1_reshaped = Array.map ~f:(repeat_elem ~n:(snd m2.shape)) m1.data in
    matrix_op ~op:Add m1_reshaped m2.data
  else if snd m2.shape = 1 then
    let m2_reshaped = Array.map ~f:(repeat_elem ~n:(snd m1.shape)) m2.data in
    matrix_op ~op:Add m1.data m2_reshaped
  else matrix_op ~op:Add m1.data m2.data

let subtract m1 m2 =
  (* if not same shape, check for array broadcast and alter shape to make subtraction work *)
  if snd m1.shape = 1 then
    let m1_reshaped = Array.map ~f:(repeat_elem ~n:(snd m2.shape)) m1.data in
    matrix_op ~op:Sub m1_reshaped m2.data
  else if snd m2.shape = 1 then
    let m2_reshaped = Array.map ~f:(repeat_elem ~n:(snd m1.shape)) m2.data in
    matrix_op ~op:Sub m1.data m2_reshaped
  else matrix_op ~op:Sub m1.data m2.data

let transpose mat =
  let num_rows, num_cols = mat.shape in
  let data = 
    Array.init num_cols ~f:(fun i ->
      Array.init num_rows ~f:(fun j -> mat.data.(j).(i)))
  in
  { data; shape = shape data }

let matmul m1 m2 =
  let m2_T = transpose m2 in
  let product v1 v2 = Array.fold2_exn ~init:0. ~f:(fun acc a b -> acc +. (a *. b)) v1 v2 in
  let data = Array.map ~f:(fun row -> Array.map ~f:(product row) m2_T.data) m1.data in
  { data; shape = shape data }


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
    let da = matmul out (transpose b) in
    let db = matmul (transpose a) out in
    da, db
  in
  let result = matmul a b in
  result, grad


(* ------------------------------- Error Functions ------------------------------- *)

let mean mat = Array.fold ~init:0. ~f:(fun acc x -> acc +. x.(0)) mat.data

let mse pred labels = 
  let n = pred.shape |> fst |> Float.of_int in
  let grad = mul ~const:(2. /. n) (matrix_op ~op:Sub pred.data labels.data) in
  let result = matrix_op ~op:Sub pred.data labels.data |> pow ~n:2. |> mean in
  result, grad

