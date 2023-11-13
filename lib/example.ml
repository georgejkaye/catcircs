open Circuits

let de_morgan_and_lhs vs =
  (fun xs -> (List.nth xs 0, List.nth xs 1)) vs
  |> pair (not_fn, not_fn)
  |> and_fn |> not_fn
  |> fun x -> [ x ]

let de_morgan_and_rhs vs =
  (fun xs -> (List.nth xs 0, List.nth xs 1)) vs |> or_fn |> fun x -> [ x ]

let de_morgan_and_lhs_circ =
  make_circuit_values de_morgan_and_lhs [ 1; 1 ] [ 1 ]

let de_morgan_and_rhs_circ =
  make_circuit_values de_morgan_and_rhs [ 1; 1 ] [ 1 ]
