open Circuits

let de_morgan_and_lhs vs =
  (fun xs -> (List.nth xs 0, List.nth xs 1)) vs
  |> pair (not_fn, not_fn)
  |> and_fn |> not_fn
  |> fun x -> [ x ]

let de_morgan_and_rhs vs =
  (fun xs -> (List.nth xs 0, List.nth xs 1)) vs |> or_fn |> fun x -> [ x ]

let de_morgan_and_lhs_circ = make_circuit_values de_morgan_and_lhs 2 1
let de_morgan_and_rhs_circ = make_circuit_values de_morgan_and_rhs 2 1

let join_dist_and_lhs vs =
  let a = List.nth vs 0 in
  let b = List.nth vs 1 in
  let c = List.nth vs 2 in
  [ join_fn (and_fn (a, b), c) ]

let join_dist_and_rhs vs =
  let a = List.nth vs 0 in
  let b = List.nth vs 1 in
  let c = List.nth vs 2 in
  [ and_fn (join_fn (a, c), join_fn (b, c)) ]

let join_dist_and_lhs_circ = make_circuit_values join_dist_and_lhs 3 1
let join_dist_and_rhs_circ = make_circuit_values join_dist_and_rhs 3 1

let join_and_or_dist_lhs vs =
  let a = List.nth vs 0 in
  let b = List.nth vs 1 in
  let c = List.nth vs 2 in
  let d = List.nth vs 3 in
  [ join_fn (and_fn (or_fn (a, b), c), d) ]

let join_and_or_dist_rhs vs =
  let a = List.nth vs 0 in
  let b = List.nth vs 1 in
  let c = List.nth vs 2 in
  let d = List.nth vs 3 in
  [
    or_fn
      ( and_fn (join_fn (a, d), join_fn (c, d)),
        and_fn (join_fn (b, d), join_fn (c, d)) );
  ]

let join_and_or_dist_lhs_circ = make_circuit_values join_and_or_dist_lhs 4 1
let join_and_or_dist_rhs_circ = make_circuit_values join_and_or_dist_rhs 4 1

let join_not_natural_lhs vs =
  let a = List.nth vs 0 in
  let b = List.nth vs 1 in
  [ not_fn (join_fn (a, b)) ]

let join_not_natural_rhs vs =
  let a = List.nth vs 0 in
  let b = List.nth vs 1 in
  [ join_fn (not_fn a, not_fn b) ]

let join_not_natural_lhs_circ = make_circuit_values join_not_natural_lhs 2 1
let join_not_natural_rhs_circ = make_circuit_values join_not_natural_rhs 2 1
