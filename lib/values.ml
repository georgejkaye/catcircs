type value = Bottom | False | True | Top
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

let string_of_list element_to_string ?(delim = ", ") ?(opening = "[")
    ?(closing = "]") xs =
  List.fold_left
    (fun (acc, i) cur ->
      let cur_string = element_to_string cur in
      let new_string =
        if i == 0 then cur_string else acc ^ delim ^ cur_string
      in
      (new_string, i + 1))
    ("", 0) xs
  |> fst
  |> fun s -> opening ^ s ^ closing

let string_of_value_list = string_of_list string_of_value

let string_of_signal s =
  if List.length s.values == 0 then "ε"
  else string_of_list string_of_value ~delim:"" ~opening:"" ~closing:"" s.values

let string_of_signal_list = string_of_list string_of_signal ~delim:", "
let value_list = [ Bottom; False; True; Top ]

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
