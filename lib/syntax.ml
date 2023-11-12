type value = Bottom | False | True | Top

let value_to_string = function
  | Bottom -> "⊥"
  | False -> "f"
  | True -> "t"
  | Top -> "⊤"

let list_to_string element_to_string ?(delim = ", ") xs =
  List.fold_left (
    fun (acc, i) -> fun cur ->
      let cur_string = element_to_string cur in
      let new_string =
          if i == 0 then cur_string else acc ^ delim ^ cur_string
      in
      (new_string, i+1)
    )
    ("", 0) xs
  |> fst

let string_of_value_list = list_to_string value_to_string ~delim:""

let string_of_value_list_list = list_to_string
  (fun vs -> if (List.length vs) == 0 then "ε" else string_of_value_list vs)

let value_list = [Bottom; False; True; Top]

let prepend_value_to_elements vss value_to_append =
    List.map (fun vs -> value_to_append :: vs ) vss

let rec enumerate_input_lists = function
| 0 -> [[]]
| w ->
    let smaller = enumerate_input_lists (w - 1) in
    print_endline (string_of_value_list_list smaller);
    List.concat
      (List.map (prepend_value_to_elements smaller) value_list)