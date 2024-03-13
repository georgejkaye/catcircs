open Printer
open Core

module type Value = sig
  type v [@@deriving enumerate, sexp, compare]

  val string_of_value : v -> string
end

type 'a signal = { values : 'a array }

let signal_length xs = Array.length xs.values

let signal_is_all v signal =
  Array.fold
    ~f:(fun acc cur -> acc && Core.phys_equal cur v)
    ~init:true signal.values

let input_is_all v =
  Array.fold ~f:(fun acc cur -> acc && signal_is_all v cur) ~init:true

let make_signal_of_all v i = { values = Array.init i ~f:(fun _ -> v) }
let make_signal_array_of_all v = Array.map ~f:(make_signal_of_all v)

module type VString = sig
  type v

  val string_of_signal : v signal -> string
  val string_of_value_array : v array -> string
  val string_of_signal_array : v signal array -> string
  val string_of_signal_array_list : v signal array list -> string
end

module ExtendString (V : Value) : VString with type v = V.v = struct
  type v = V.v

  let string_of_signal s =
    if phys_equal (Array.length s.values) 0 then "ε"
    else
      string_of_array V.string_of_value ~delim:"" ~opening:"" ~closing:""
        s.values

  let string_of_value_array vs = string_of_array V.string_of_value vs
  let string_of_signal_array ss = string_of_array string_of_signal ss

  let string_of_signal_array_list sss =
    string_of_list string_of_signal_array sss
end

module ExtendSet (V : Value) = Set.Make (struct
  type t = V.v

  let compare = V.compare_v
  let t_of_sexp = V.v_of_sexp
  let sexp_of_t = V.sexp_of_v
end)

module type VEnum = sig
  type v

  val enumerate_value_arrays : int -> v array list
  val enumerate_signals : int -> v signal list
  val enumerate_inputs : int list -> v signal array list
  val widths_from_inputs : v signal array -> int list
end

module ExtendEnum (V : Value) : VEnum with type v := V.v = struct
  let prepend_value_to_signals elems value_to_append =
    List.map
      ~f:(fun vs -> { values = Array.append [| value_to_append |] vs.values })
      elems

  let prepend_value_to_elements elems value_to_append =
    List.map ~f:(fun vs -> Array.append [| value_to_append |] vs) elems

  let rec enumerate_value_arrays = function
    | 0 -> [ [||] ]
    | w ->
        let smaller = enumerate_value_arrays (w - 1) in
        List.concat_map ~f:(prepend_value_to_elements smaller) V.all_of_v

  let rec enumerate_signals = function
    | 0 -> [ { values = [||] } ]
    | w ->
        let smaller = enumerate_signals (w - 1) in
        List.concat_map ~f:(prepend_value_to_signals smaller) V.all_of_v

  let rec enumerate_combinations = function
    | [] -> [ [] ]
    | xs :: yss ->
        let smaller = enumerate_combinations yss in
        List.concat_map ~f:(fun x -> List.map ~f:(fun ys -> x :: ys) smaller) xs

  let enumerate_inputs wss =
    let inputs = List.map ~f:enumerate_signals wss in
    let combs = enumerate_combinations inputs in
    List.map ~f:List.to_array combs

  let widths_from_inputs wss =
    Array.to_list (Array.map ~f:(fun ws -> Array.length ws.values) wss)
end
