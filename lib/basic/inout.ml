open Value
open Printer

type 'a io_cmp = { input : 'a signal array; outputs : 'a signal array array }

module type VInOut = sig
  type v

  val all_equal : v io_cmp -> bool
  val some_equal : int list -> v io_cmp -> bool
  val some_of_list_equal : int list -> v io_cmp list -> bool
  val all_of_list_equal : v io_cmp list -> bool
  val string_of_io_cmp : v io_cmp -> string
  val string_of_io_cmp_list : v io_cmp list -> string
end

module ExtendInOut (V : Value) : VInOut with type v = V.v = struct
  type v = V.v

  module VString = ExtendString (V)

  let check_indices_equal lhs rhs =
    List.fold_left
      (fun acc i ->
        let l = lhs.(i) in
        let r = rhs.(i) in
        let l_string = VString.string_of_signal l in
        let r_string = VString.string_of_signal r in
        let result = String.equal l_string r_string in
        acc && result)
      true

  let some_equal indices io =
    Array.fold_left
      (fun acc output_to_consider ->
        let result =
          check_indices_equal io.outputs.(0) output_to_consider indices
        in
        acc && result)
      true io.outputs

  let some_of_list_equal indices =
    List.fold_left (fun acc cur -> acc && some_equal indices cur) true

  let all_equal io =
    some_equal (List.init (Array.length io.outputs - 1) (fun i -> i + 1)) io

  let all_of_list_equal =
    List.fold_left (fun acc cur -> acc && all_equal cur) true

  let string_of_io_cmp io =
    VString.string_of_signal_array io.input
    ^ " || "
    ^ Printer.string_of_array VString.string_of_signal_array ~delim:" | "
        ~opening:"" ~closing:"" io.outputs
    ^
    if Array.length io.outputs < 2 then ""
    else if all_equal io then "  O"
    else "  X"

  let string_of_io_cmp_list =
    string_of_list string_of_io_cmp ~delim:"\n" ~opening:"" ~closing:""
end
