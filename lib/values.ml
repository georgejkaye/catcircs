open Printer
open Base

module type V = sig
  type v

  val string_of_value : v -> string
  val values : v list
end

type 'a signal = { values : 'a array }

type 'a io_cmp = {
  input : 'a signal array;
  lhs : 'a signal array;
  rhs : 'a signal array;
}

type 'a circuit = {
  fn : 'a signal array -> 'a signal array;
  arity : int list;
  coarity : int list;
}

module type VF = sig
  type v

  val string_of_signal : v signal -> string
  val string_of_value_array : v array -> string
  val string_of_signal_array : v signal array -> string
  val string_of_io : v io_cmp -> string
  val string_of_io_cmp_list : v io_cmp list -> string
  val enumerate_signals : int -> v signal list
  val enumerate_inputs : int list -> v signal array list
  val compare : v circuit -> v circuit -> v io_cmp list
  val string_of_outputs : v circuit -> string
  val string_of_comparison : v circuit -> v circuit -> string
end

module ExtendValue (V : V) : VF with type v := V.v = struct
  let string_of_signal s =
    if phys_equal (Array.length s.values) 0 then "ε"
    else
      string_of_array V.string_of_value ~delim:"" ~opening:"" ~closing:""
        s.values

  let string_of_value_array vs = string_of_array V.string_of_value vs
  let string_of_signal_array ss = string_of_array string_of_signal ss

  let string_of_io io =
    string_of_signal_array io.input
    ^ " || "
    ^ string_of_signal_array io.lhs
    ^ " | "
    ^ string_of_signal_array io.rhs
    ^ if Array.equal phys_equal io.lhs io.rhs then "  O" else "  X "

  let string_of_io_cmp_list =
    string_of_list string_of_io ~delim:"\n" ~opening:"" ~closing:""

  let prepend_value_to_elements vss value_to_append =
    List.map
      ~f:(fun vs ->
        { values = List.to_array (value_to_append :: List.of_array vs.values) })
      vss

  let rec enumerate_signals = function
    | 0 -> [ { values = [||] } ]
    | w ->
        let smaller = enumerate_signals (w - 1) in
        List.concat_map ~f:(prepend_value_to_elements smaller) V.values

  let rec enumerate_combinations = function
    | [] -> [ [] ]
    | xs :: yss ->
        let smaller = enumerate_combinations yss in
        List.concat_map ~f:(fun x -> List.map ~f:(fun ys -> x :: ys) smaller) xs

  let enumerate_inputs wss =
    let inputs = List.map ~f:enumerate_signals wss in
    let combs = enumerate_combinations inputs in
    List.map ~f:List.to_array combs

  let compare c1 c2 =
    enumerate_inputs c1.arity
    |> List.map ~f:(fun ss ->
           let lhs = c1.fn ss in
           let rhs = c2.fn ss in
           { input = ss; lhs; rhs })

  let string_of_outputs c =
    let inputs = enumerate_inputs c.arity in
    let outputs = List.map ~f:(fun ss -> c.fn ss) inputs in
    let rows =
      List.map2
        ~f:(fun ins outs ->
          string_of_signal_array ins ^ " | " ^ string_of_signal_array outs)
        inputs outputs
    in
    match rows with
    | Ok xs ->
        string_of_list (fun x -> x) ~opening:"" ~closing:"" ~delim:"\n" xs
    | Unequal_lengths -> failwith "never"

  let get_errors ios =
    List.rev
      (List.fold_left ~init:[]
         ~f:(fun acc io ->
           if Array.equal phys_equal io.lhs io.rhs then acc else io :: acc)
         ios)

  let string_of_comparison c1 c2 =
    let comparison = compare c1 c2 in
    let row_length =
      List.fold_left ~init:0 ~f:max
        (List.map ~f:(fun cmp -> string_of_io cmp |> String.length) comparison)
    in
    let equals_lines = String.init row_length ~f:(fun _ -> '=') in
    let table = string_of_io_cmp_list comparison in
    let errors = get_errors comparison in
    let error_text =
      if phys_equal (List.length errors) 0 then "All clear!"
      else "Errors:\n" ^ string_of_io_cmp_list errors
    in
    equals_lines ^ "\n" ^ table ^ "\n" ^ equals_lines ^ "\n" ^ error_text
end
