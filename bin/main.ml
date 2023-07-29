open Stdio
open Mlnet


let inputs = Matrix.from_array [| [| 1.; 2.; 3. |]; [| 4.; 5.; 6. |]; [| 7.; 8.; 9.|]; [| 10.; 11.; 12.|]|]
let targets = Matrix.from_array [| [| 0.|]; [| 1. |]; [| 2. |]; [| 0. |]|]

let model = Nn.chain [ 
  Nn.linear 3 3; 
  Nn.relu;
  Nn.linear 3 3;
  Nn.relu; 
  Nn.linear 3 3; 
]
let loss_fn = Nn.cross_entropy_loss
let optimizer = Optim.sgd model

let () =
  Train.fit model inputs targets ~epochs:500 ~loss_fn ~optimizer;
  let out, _ = model.forward inputs in
  printf "Accuracy=%f" (Utils.accuracy (Utils.argmax out.data) targets.data)
  