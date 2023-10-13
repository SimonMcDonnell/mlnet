open Base
open Stdio
open Mlnet


let read_csv_file filename =
  let csv = Csv.load filename |> Csv.to_array in
  Array.map csv ~f:(fun row -> Array.map row ~f:(fun x -> Float.of_string x))

let x_train, y_train = read_csv_file "bin/train_images.csv", read_csv_file "bin/train_labels.csv"
let x_test, y_test = read_csv_file "bin/test_images.csv", read_csv_file "bin/test_labels.csv"

let x_train, y_train, x_test, y_test = 
    Matrix.from_array x_train, Matrix.from_array y_train, Matrix.from_array x_test, Matrix.from_array y_test

let model = Nn.chain [ 
  Nn.linear (snd x_train.shape) 100; 
  Nn.relu;
  Nn.linear 100 50; 
  Nn.relu; 
  Nn.linear 50 10; 
]
let loss_fn = Nn.cross_entropy_loss
let optimizer = Optim.sgd ~lr:0.2 model

let () =
  print_endline "Training model";
  Train.fit model x_train y_train ~epochs:15 ~loss_fn ~optimizer;
  let out, _ = model.forward x_test in
  printf "Accuracy=%f" (Utils.accuracy (Utils.argmax out.data) y_test.data)
  