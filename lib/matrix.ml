open Base

type t = { data : float array array; shape : int * int } [@@deriving show]

let shape data = (Array.length data, Array.length data.(0))

let create data = { data; shape = shape data }

let sum_same_size d1 d2 =
  let add_lists d1 d2 = Array.map2_exn ~f:(fun x y -> x +. y) d1 d2 in
  let summed = Array.map2_exn ~f:add_lists d1 d2 in
  { data = summed; shape = shape summed }

let sum m1 m2 =
  let repeat_elem ~n elem =
    match elem with 
    | [| x |] -> Array.init n ~f:(fun _ -> x) 
    | lst -> lst
  in
  (* if not same shape, check for array broadcast and alter shape to make addition work *)
  if snd m1.shape = 1 then
    let m1_reshaped = Array.map m1.data ~f:(repeat_elem ~n:(snd m2.shape)) in
    sum_same_size m1_reshaped m2.data
  else if snd m2.shape = 1 then
    let m2_reshaped = Array.map m2.data ~f:(repeat_elem ~n:(snd m1.shape)) in
    sum_same_size m1.data m2_reshaped
  else sum_same_size m1.data m2.data

let transpose mat =
  let num_rows = fst mat.shape in
  let num_cols = snd mat.shape in
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

(* let dot a b =
   let backward out =
     let da = matmul out (transpose b) in
     let db = matmul (transpose a) out in
     da, db
   in
   let result = matmul a b in
   result, backward *)
