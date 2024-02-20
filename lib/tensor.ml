open Base
open Owl


type t = Mat.mat


(* INITIALIZATION *)

let from_array arr = Mat.of_arrays arr

let shape mat = Mat.shape mat

let get mat row col = Mat.get mat row col

let print mat = Mat.print mat

let show mat = Stdlib.Format.asprintf "%a" Owl_pretty.pp_dsnda mat

let ones n_rows n_cols = Mat.ones n_rows n_cols

let zeros n_rows n_cols = Mat.zeros n_rows n_cols

(* use 'He' initialization technique. *)
let he_init n_rows n_cols = 
  (* create gaussian sample from uniform sample using Box Muller transform *)
  let uni_to_gauss u0 u1 = 
    (Float.sqrt (-2.0 *. Float.log u0)) *. (Float.cos (2.0 *. Float.pi *. u1))
  in
  Mat.init n_rows n_cols (fun _ ->
    (Float.sqrt (2.0 /. (Float.of_int n_rows))) *. 
    (uni_to_gauss (Random.float 1.) (Random.float 1.)))


(* UNARY OPS *)

let scalar_pow ~n mat = Mat.scalar_pow n mat 

let scalar_mul ~n mat = Mat.scalar_mul n mat

let scalar_add ~n mat = Mat.scalar_add n mat

let scalar_sub ~n mat = Mat.scalar_sub n mat

let sigmoid mat = Mat.sigmoid mat

let softmax ?(axis=(-1)) mat = Mat.softmax ~axis mat

let map ~f mat = Mat.map f mat

let mapi ~f mat = Mat.mapi f mat

let map2 ~f m1 m2 = Mat.map2 f m1 m2

let neg mat = Mat.neg mat

let exp mat = Mat.exp mat

let log mat = Mat.log mat

let transpose mat = Mat.transpose mat

let sum_rows mat = Mat.sum_rows mat

let sum_cols mat = Mat.sum_cols mat

let mean mat = Mat.mean' mat

  
(* BINARY OPS *)

let add m1 m2 = Mat.add m1 m2

let sub m1 m2 = Mat.sub m1 m2

let dot m1 m2 = Mat.dot m1 m2

let mul m1 m2 = Mat.mul m1 m2

let div m1 m2 = Mat.div m1 m2