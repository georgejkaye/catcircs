open Printer

type value = Bottom | False | True | Top

let value_list = [ Bottom; False; True; Top ]

type signal = { values : value list }

let string_of_value = function
  | Bottom -> "⊥"
  | False -> "f"
  | True -> "t"
  | Top -> "⊤"

let string_of_boolsq_value = function
  | Bottom -> "00"
  | False -> "01"
  | True -> "10"
  | Top -> "11"

let string_of_value_list = string_of_list string_of_value

let string_of_signal s =
  if List.length s.values == 0 then "ε"
  else string_of_list string_of_value ~delim:"" ~opening:"" ~closing:"" s.values

let string_of_signal_list = string_of_list string_of_signal ~delim:", "
let fork v = (v, v)

let join_fn = function
  | Bottom, w -> w
  | v, Bottom -> v
  | Top, _ -> Top
  | _, Top -> Top
  | True, False -> Top
  | False, True -> Top
  | True, True -> True
  | False, False -> False

let and_fn = function
  | False, _ -> False
  | _, False -> False
  | True, w -> w
  | v, True -> v
  | Bottom, Bottom -> Bottom
  | Bottom, Top -> False
  | Top, Bottom -> False
  | Top, Top -> Top

let or_fn = function
  | True, _ -> True
  | _, True -> True
  | False, w -> w
  | v, False -> v
  | Bottom, Bottom -> Bottom
  | Bottom, Top -> True
  | Top, Bottom -> True
  | Top, Top -> Top

let not_fn = function
  | Bottom -> Bottom
  | True -> False
  | False -> True
  | Top -> Top

let prepend_value_to_elements vss value_to_append =
  List.map (fun vs -> { values = value_to_append :: vs.values }) vss

let rec enumerate_signals = function
  | 0 -> [ { values = [] } ]
  | w ->
      let smaller = enumerate_signals (w - 1) in
      List.concat_map (prepend_value_to_elements smaller) value_list

let rec enumerate_combinations = function
  | [] -> [ [] ]
  | xs :: yss ->
      let smaller = enumerate_combinations yss in
      List.concat_map (fun x -> List.map (fun ys -> x :: ys) smaller) xs

let enumerate_inputs wss =
  List.map enumerate_signals wss |> enumerate_combinations
