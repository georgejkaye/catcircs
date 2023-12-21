open Printer
open Core

module type Value = sig
  type v [@@deriving enumerate, sexp, compare]

  val string_of_value : v -> string
  val join : v -> v -> v
  val meet : v -> v -> v
end

type 'a signal = { values : 'a array }

module type VString = sig
  type v

  val string_of_signal : v signal -> string
  val string_of_value_array : v array -> string
  val string_of_signal_array : v signal array -> string
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
end

module ExtendSet (V : Value) = Set.Make (struct
  type t = V.v

  let compare = V.compare_v
  let t_of_sexp = V.v_of_sexp
  let sexp_of_t = V.sexp_of_v
end)

module type VEnum = sig
  type v

  val enumerate_signals : int -> v signal list
  val enumerate_inputs : int list -> v signal array list
end

module ExtendEnum (V : Value) : VEnum with type v := V.v = struct
  let prepend_value_to_elements value_to_append =
    List.map ~f:(fun vs ->
        { values = List.to_array (value_to_append :: List.of_array vs.values) })

  let rec enumerate_signals = function
    | 0 -> [ { values = [||] } ]
    | w ->
        let smaller = enumerate_signals (w - 1) in
        List.concat_map
          ~f:(fun x -> prepend_value_to_elements x smaller)
          V.all_of_v

  let rec enumerate_combinations = function
    | [] -> [ [] ]
    | xs :: yss ->
        let smaller = enumerate_combinations yss in
        List.concat_map ~f:(fun x -> List.map ~f:(fun ys -> x :: ys) smaller) xs

  let enumerate_inputs wss =
    let inputs = List.map ~f:enumerate_signals wss in
    let combs = enumerate_combinations inputs in
    List.map ~f:List.to_array combs
end
