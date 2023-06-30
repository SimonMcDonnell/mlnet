open Base
open Stdio
open Mlnet


let inputs = Matrix.from_array [| [| 1.; 2.; 3. |]; [| 4.; 5.; 6. |]; [| 7.; 8.; 9.|]; [| 10.; 11.; 12.|]|]
let targets = Matrix.from_array [| [| 10.|]; [| 30. |]; [| 40.|]; [| 10.|]|]
let loss_fn = Nn.mse_loss
let model = Nn.sequential [ 
  Nn.linear 3 3; 
  Nn.linear 3 2;
  Nn.relu; 
  Nn.linear 2 1; 
]

let out, backprop = model.forward inputs
let loss, grad = loss_fn out targets

let () = 
  printf "%f\n" loss;
  Matrix.show grad |> print_endline;
  Nn.show model |> print_endline;
  backprop grad;
  Nn.show model |> print_endline