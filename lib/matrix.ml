open Base


type t = { data : float array array; shape : int * int } [@@deriving show]
type op = Add | Sub | Mul


let shape data = (Array.length data, Array.length data.(0))

let from_array data = { data; shape = shape data }

let ones n_rows n_cols = 
  Array.init ~f:(fun _ -> Array.init ~f:(fun _ -> 1.) n_cols) n_rows |> from_array

let zeros n_rows n_cols = 
  Array.init ~f:(fun _ -> Array.init ~f:(fun _ -> 0.) n_cols) n_rows |> from_array

let he_init n_rows n_cols = 
  (* use 'He' initialization technique. *)
  let uni_to_gauss u0 u1 = 
    (* create gaussian sample from uniform sample using Box Muller transform *)
    (Float.sqrt (-2.0 *. Float.log u0)) *. (Float.cos (2.0 *. Float.pi *. u1))
  in
  let data = 
    Array.init n_rows ~f:(fun _ -> 
      Array.init n_cols ~f:(fun _ -> 
        let n_in = Float.of_int n_rows in
        (Float.sqrt (2.0 /. n_in)) *. (uni_to_gauss (Random.float 1.) (Random.float 1.))
      )
    )
  in
  { data; shape = (n_rows, n_cols)}

let apply_elem ~f data = Array.map ~f:(fun row -> Array.map ~f row) data

let pow mat ~n = 
  let result = apply_elem ~f:(fun x -> x **. n) mat.data in
  { data = result; shape = mat.shape }

let mul mat ~const = 
  let result = apply_elem ~f:(fun x -> x *. const) mat.data in
  { data = result; shape = mat.shape }

let sigmoid mat = 
  let sig_fn x = 1. /. (1. +. Float.exp (Float.neg x)) in
  let result = apply_elem ~f:(fun x -> sig_fn x) mat.data in
  { data = result; shape = mat.shape }

let apply ~f mat = 
  let data = apply_elem ~f mat.data in
  { data; shape = mat.shape }

let replace mat ~cond ~new_val = 
  let result = Array.map ~f:(fun row -> 
    Array.map ~f:(fun i -> if cond i then new_val else i) row) mat.data
  in
  { data = result; shape = mat.shape }

let matrix_op ~op d1 d2 = 
  (* add or subtract matrices *)
  let op_fn ~op d1 d2 = 
    match op with
    | Add -> Array.map2_exn ~f:(fun x y -> x +. y) d1 d2
    | Sub -> Array.map2_exn ~f:(fun x y -> x -. y) d1 d2
    | Mul -> Array.map2_exn ~f:(fun x y -> x *. y) d1 d2
  in
  Array.map2_exn ~f:(op_fn ~op) d1 d2

let transp data =
  let num_rows, num_cols = shape data in
  Array.init num_cols ~f:(fun i ->
    Array.init num_rows ~f:(fun j -> data.(j).(i)))

let transpose mat = 
  { data = transp mat.data; shape = (snd mat.shape, fst mat.shape) }

let repeat_elem ~n elem = Array.init n ~f:(fun _ -> elem)

let sum m1 m2 =
  (* if not same shape, check for array broadcast and alter shape to make addition work *)
  if fst m1.shape = 1 then
    let m1_reshaped = Array.map ~f:(repeat_elem ~n:(fst m2.shape)) m1.data.(0) |> transp in
    let result = matrix_op ~op:Add m1_reshaped m2.data in
    { data = result; shape = m2.shape }
  else if fst m2.shape = 1 then
    let m2_reshaped = Array.map ~f:(repeat_elem ~n:(fst m1.shape)) m2.data.(0) |> transp in
    let result = matrix_op ~op:Add m1.data m2_reshaped in
    { data = result; shape = m1.shape }
  else 
    let result = matrix_op ~op:Add m1.data m2.data in
    { data = result; shape = m1.shape }

let sub m1 m2 =
  (* if not same shape, check for array broadcast and alter shape to make subtraction work *)
  if fst m1.shape = 1 then
    let m1_reshaped = Array.map ~f:(repeat_elem ~n:(fst m2.shape)) m1.data.(0) |> transp in
    let result = matrix_op ~op:Sub m1_reshaped m2.data in
    { data = result; shape = m2.shape }
  else if fst m2.shape = 1 then
    let m2_reshaped = Array.map ~f:(repeat_elem ~n:(fst m1.shape)) m2.data.(0) |> transp in
    let result = matrix_op ~op:Sub m1.data m2_reshaped in
    { data = result; shape = m1.shape }
  else 
    let result = matrix_op ~op:Sub m1.data m2.data in
    { data = result; shape = m1.shape }

let matmul m1 m2 =
  let m2_T = transp m2.data in
  let product v1 v2 = Array.fold2_exn ~init:0. ~f:(fun acc a b -> acc +. (a *. b)) v1 v2 in
  let result = Array.map ~f:(fun row -> Array.map ~f:(product row) m2_T) m1.data in
  { data = result; shape = (fst m1.shape, snd m2.shape) }

let multiply m1 m2 = 
  let result = matrix_op ~op:Mul m1.data m2.data in
  { data = result; shape = m1.shape }

let divide m1 m2 = multiply m1 (apply ~f:(fun x -> 1. /. x) m2)