open Base


let argmax logits = 
  let max_ind (arr: float array) = 
    let zipped = Array.zip_exn arr (Array.init (Array.length arr) ~f:(fun i -> Float.of_int i)) in
    let _, ind = Array.fold zipped ~init:(Float.neg_infinity, -1.) ~f:(fun (a, b) (x, y) ->
      if Float.(>) x a then (x, y) else (a, b))
    in
    [| ind |]
  in
  Array.map logits ~f:max_ind

let accuracy pred target = 
  let sum = 
    Array.map2_exn pred target ~f:(fun p t -> if Float.equal p.(0) t.(0) then 1. else 0.)
    |> Array.fold ~init:0. ~f:(+.)
  in
  let l = Array.length pred |> Float.of_int in
  sum /. l