open Values

let value_fn_to_signal_fn fn ss =
  Array.map (fun s -> s.values.(0)) ss
  |> fn
  |> Array.map (fun v -> { values = [| v |] })

let make_circuit_values fn ar coar =
  {
    fn = value_fn_to_signal_fn fn;
    arity = List.init ar (fun _ -> 1);
    coarity = List.init coar (fun _ -> 1);
  }

let make_circuit_signals fn ar coar = { fn; arity = ar; coarity = coar }
