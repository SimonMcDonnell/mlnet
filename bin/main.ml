open Base
open Stdio
open Mlnet

let mat = Matrix.create [| [| 1.; 2. |]; [| 1.; 2. |] |]
let mat2 = Matrix.create [| [| 10. |]; [| 5. |] |]

let () =
  Matrix.matmul mat mat |> Matrix.sum mat2 |> Matrix.transpose |> Matrix.show
  |> print_endline
