open Values
open Circuits
open Printer

type 'a row = 'a list * 'a list
type 'a table = { inputs : int; outputs : int; table : 'a row list }

let string_of_row_segment fn =
  string_of_list fn ~delim:" " ~opening:"" ~closing:""

let string_of_value_row_segment = string_of_row_segment string_of_value

let string_of_row fn (inputs, outputs) =
  string_of_row_segment fn inputs ^ " | " ^ string_of_row_segment fn outputs

let string_of_table fn t =
  List.fold_left
    (fun (i, acc) cur ->
      let str = string_of_row fn cur in
      if i == 0 then (1, str) else (i + 1, acc ^ "\n" ^ str))
    (0, "") t.table
  |> snd

let string_of_belnap_table = string_of_table string_of_value
let string_of_boolsq_table = string_of_table string_of_boolsq_value

let table_to_many_to_table_to_one t k =
  let rows =
    List.map (fun (inputs, outputs) -> (inputs, [ List.nth outputs k ])) t.table
  in
  { inputs = t.inputs; outputs = 1; table = rows }

let table_to_many_to_tables_to_one t =
  List.init t.outputs (fun i -> table_to_many_to_table_to_one t i)

let map_row fn (inputs, outputs) = (List.map fn inputs, List.map fn outputs)

let map_table fn t =
  {
    inputs = t.inputs;
    outputs = t.outputs;
    table = List.map (map_row fn) t.table;
  }

let get_dnf_clause to_bool conj conj_init row vs =
  List.fold_left
    (fun (i, acc) cur ->
      let conjed = if to_bool cur then conj (acc, List.nth vs i) else acc in
      (i + 1, conjed))
    (0, conj_init) row
  |> snd

let get_dnf to_bool disj disj_init conj conj_init rows vs =
  List.fold_left
    (fun acc (inputs, output) ->
      if to_bool (List.nth output 0) then
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
  List.fold_left
    (fun acc cur ->
      let a, b = exp cur in
      b :: a :: acc)
    [] inputs
  |> List.rev

let explode_input_translate_output explode_exp translate_exp (inputs, output) =
  (explode_inputs explode_exp inputs, List.map translate_exp output)

let explode_input_translate_output_rows explode_exp translate_exp =
  List.map (explode_input_translate_output explode_exp translate_exp)

let explode_input_translate_output_table explode_exp translate_exp t =
  {
    inputs = t.inputs * 2;
    outputs = t.outputs;
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

let table_to_one_to_function t =
  if t.outputs != 1 then failwith "table_to_one_to_function: needs one output"
  else
    let left_table = left_explode_table t in
    let right_table = right_explode_table t in
    let left_dnf =
      get_dnf left_bool or_fn Bottom and_fn True left_table.table
    in
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
