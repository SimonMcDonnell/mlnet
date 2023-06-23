open Base
open Stdio
open Mlnet


let inputs = Matrix.from_array [| [| 1.; 1.; 2. |]; [| 1.; 3.; 4. |]; [| 1.; 5.; 6.|]; [| 1.; 7.; 8.|]|]
let dummy_grad = Matrix.ones 4 1

let model = Nn.sequential [ Nn.linear 3 2; Nn.linear 2 1 ]

let () = 
  let pred, backprop = model.forward inputs in
  Matrix.show pred |> print_endline;
  Nn.show model |> print_endline;
  backprop dummy_grad;
  Nn.show model |> print_endline