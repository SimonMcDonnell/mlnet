open Base
open Stdio
open Mlnet


let inputs = Matrix.from_array [| [| 1.; 2.; 3. |]; [| 4.; 5.; 6. |]; [| 7.; 8.; 9.|]; [| 10.; 11.; 12.|]|]
let targets = Matrix.from_array [| [| 10.|]; [| 30. |]; [| 40.|]; [| 10.|]|]

let model = Nn.chain [ 
  Nn.linear 3 3; 
  Nn.relu;
  Nn.linear 3 2;
  Nn.relu; 
  Nn.linear 2 1; 
]
let loss_fn = Nn.mse_loss
let optimizer = Optim.sgd model

let () =
  Train.fit model inputs targets ~epochs:300 ~loss_fn ~optimizer;
  let out, _ = model.forward inputs in
  Matrix.show out |> print_endline