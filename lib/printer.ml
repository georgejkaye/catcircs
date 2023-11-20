let string_of_iterable_element string_of_element ?(delim = ", ") (acc, i) cur =
  let cur_string = string_of_element cur in
  let new_string = if i == 0 then cur_string else acc ^ delim ^ cur_string in
  (new_string, i + 1)

let string_of_iterable fold string_of_element ?(delim = ", ") ?(opening = "[")
    ?(closing = "]") xs =
  fold (string_of_iterable_element string_of_element ~delim) ("", 0) xs |> fst
  |> fun s -> opening ^ s ^ closing

let string_of_list string_of_element =
  string_of_iterable List.fold_left string_of_element

let string_of_array string_of_element =
  string_of_iterable Array.fold_left string_of_element
