open Belnap
open Table

let string_of_belnap_table = string_of_table string_of_value
let string_of_boolsq_table = string_of_table string_of_boolsq_value

let get_dnf_clause to_bool conj conj_init row vs =
  Array.fold_left
    (fun (i, acc) cur ->
      let conjed = if to_bool cur then conj (acc, vs.(i)) else acc in
      (i + 1, conjed))
    (0, conj_init) row
  |> snd

let get_dnf to_bool disj disj_init conj conj_init rows vs =
  Array.fold_left
    (fun acc (inputs, output) ->
      if to_bool output then
        let clause = get_dnf_clause to_bool conj conj_init inputs vs in
        disj (acc, clause)
      else acc)
    disj_init rows

let left_zero v = and_fn (Bottom, v)

let left_bool = function
  | Bottom -> false
  | True -> true
  | False -> false
  | Top -> true

let belnap_to_left = function
  | Bottom -> Bottom
  | True -> True
  | False -> Bottom
  | Top -> True

let right_zero v = or_fn (Bottom, v)

let right_bool = function
  | Bottom -> false
  | True -> false
  | False -> true
  | Top -> true

let belnap_to_right = function
  | Bottom -> Bottom
  | True -> Bottom
  | False -> False
  | Top -> False

let explode_inputs exp inputs =
  Array.init
    (Array.length inputs * 2)
    (fun i ->
      let candidate = inputs.(i / 2) in
      let l, r = exp candidate in
      if i mod 2 == 0 then l else r)

let explode_input_translate_output explode_exp translate_exp (inputs, output) =
  (explode_inputs explode_exp inputs, translate_exp output)

let explode_input_translate_output_rows explode_exp translate_exp =
  Array.map (explode_input_translate_output explode_exp translate_exp)

let explode_input_translate_output_table explode_exp translate_exp
    (t : 'a tableone) =
  {
    inputs = t.inputs * 2;
    table =
      explode_input_translate_output_rows explode_exp translate_exp t.table;
  }

let belnap_to_btwo = function
  | Bottom -> (false, false)
  | False -> (false, true)
  | True -> (true, false)
  | Top -> (true, true)

let btwo_to_belnap_left b = if b then True else Bottom
let btwo_to_belnap_right b = if b then False else Bottom

let belnap_btwo_explode fn v =
  let l, r = belnap_to_btwo v in
  (fn l, fn r)

let left_explode = belnap_btwo_explode btwo_to_belnap_left
let right_explode = belnap_btwo_explode btwo_to_belnap_right
let left_explode_inputs = explode_inputs left_explode
let right_explode_inputs = explode_inputs right_explode

let left_explode_table t =
  explode_input_translate_output_table left_explode belnap_to_left t

let right_explode_table t =
  explode_input_translate_output_table right_explode belnap_to_right t

let table_to_one_to_function (t : 'a tableone) =
  let left_table = left_explode_table t in
  let right_table = right_explode_table t in
  let left_dnf = get_dnf left_bool or_fn Bottom and_fn True left_table.table in
  let right_dnf =
    get_dnf right_bool and_fn Bottom or_fn False right_table.table
  in
  fun vs ->
    let left_inputs = left_explode_inputs vs in
    let left_component = left_dnf left_inputs in
    let right_inputs = right_explode_inputs vs in
    let right_component = right_dnf right_inputs in
    join_fn (left_component, right_component)

let table_to_many_to_function t =
  let one_tables = table_to_many_to_tables_to_one t in
  let fns = List.map table_to_one_to_function one_tables in
  fun vs -> List.fold_left (fun acc f -> f vs :: acc) [] fns |> List.rev
