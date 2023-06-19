open Base
open Stdio
open Mlnet


let inputs = Matrix.create [| [| 1.; 1.; 2. |]; [| 1.; 3.; 4. |]; [| 1.; 5.; 6.|]; [| 1.; 7.; 8.|]|]
let weights = ref @@ Matrix.create [| [| 1.7 |]; [| 1.5 |]; [| 1.2 |] |]
let targets = Matrix.create [| [| 10. |]; [| 20. |]; [| 30. |]; [| 40. |]|]

let lr = 0.001
let print_error error = error |> Float.to_string |> print_endline

let () =
  for i = 1 to 10000 do
    print_endline (Int.to_string i);
    let h1, grad_w = Matrix.dot inputs !weights in
    let error, grad_err = Matrix.mse h1 targets in
    let dw =  snd (grad_w grad_err) in
    let result, _ = Matrix.sub (!weights) (Matrix.mul ~const:lr dw) in
    weights := result;
    print_error error
  done;
  let preds, _ = Matrix.dot inputs !weights in
  preds |> Matrix.show |> print_endline
