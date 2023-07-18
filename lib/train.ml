open Stdio


let fit model inputs targets ~epochs ~loss_fn ~optimizer = 
  for epoch = 1 to epochs do
    let out, backprop = model.Nn.forward inputs in
    let loss, grad = loss_fn out targets in
    backprop grad;
    optimizer.Optim.step model;
    if epoch mod 10 = 0 then
      printf "Epoch %d --- loss=%f\n" epoch loss;
  done;