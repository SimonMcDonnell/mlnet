open Base
open Stdio
open Mlnet


let inputs = Matrix.from_array [| [| 1.; 1.; 2. |]; [| 1.; 3.; 4. |]; [| 1.; 5.; 6.|]; [| 1.; 7.; 8.|]|]
let out = Matrix.ones 4 1 |> Matrix.mul ~const: 5.

let () = 
  let model = Nn.linear 3 1 in
  let result, backprop = model.forward model inputs in
  Matrix.show result |> print_endline;
  Nn.show model |> print_endline;
  let _ = backprop out in
  Nn.show model |> print_endline