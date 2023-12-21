open Printer
open Value

type 'a row = 'a array * 'a array
type 'a tablemany = { inputs : int; outputs : int; tablem : 'a row array }
type 'a tableone = { inputs : int; table : ('a array * 'a) array }

module type VTable = sig
  type v

  val string_of_row : v row -> string
  val string_of_table : v tablemany -> string
end

module ExtendTable (V : Value) : VTable with type v = V.v = struct
  type v = V.v

  let string_of_row_segment =
    string_of_array V.string_of_value ~delim:" " ~opening:"" ~closing:""

  let string_of_row (inputs, outputs) =
    string_of_row_segment inputs ^ " | " ^ string_of_row_segment outputs

  let string_of_table t =
    let _, str =
      Array.fold_left
        (fun (i, acc) cur ->
          let str = string_of_row cur in
          if i == 0 then (1, str) else (i + 1, acc ^ "\n" ^ str))
        (0, "") t.tablem
    in
    str
end

let table_to_many_to_table_to_one (t : 'a tablemany) k =
  let rows =
    Array.map (fun (inputs, outputs) -> (inputs, outputs.(k))) t.tablem
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
