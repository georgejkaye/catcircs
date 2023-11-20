open Printer

type 'a row = 'a array * 'a array
type 'a tablemany = { inputs : int; outputs : int; table : 'a row array }
type 'a tableone = { inputs : int; table : ('a array * 'a) array }

let string_of_row_segment fn =
  string_of_array fn ~delim:" " ~opening:"" ~closing:""

let string_of_row fn (inputs, outputs) =
  string_of_row_segment fn inputs ^ " | " ^ string_of_row_segment fn outputs

let string_of_table fn (t : 'a tablemany) =
  Array.fold_left
    (fun (i, acc) cur ->
      let str = string_of_row fn cur in
      if i == 0 then (1, str) else (i + 1, acc ^ "\n" ^ str))
    (0, "") t.table
  |> snd

let table_to_many_to_table_to_one (t : 'a tablemany) k =
  let rows =
    Array.map (fun (inputs, outputs) -> (inputs, outputs.(k))) t.table
  in
  { inputs = t.inputs; table = rows }

let table_to_many_to_tables_to_one t =
  List.init t.outputs (fun i -> table_to_many_to_table_to_one t i)

let make_dnf_clause vs row =
  Array.fold_left
    (fun (i, acc) cur ->
      let conjed = if cur then acc && vs.(i) else acc in
      (i + 1, conjed))
    (0, true) row
  |> snd

let make_dnf vs =
  Array.fold_left
    (fun acc (inputs, output) ->
      if output then
        let clause = make_dnf_clause vs inputs in
        acc || clause
      else acc)
    false

let make_table_dnf (t : bool tableone) vs = make_dnf vs t.table
