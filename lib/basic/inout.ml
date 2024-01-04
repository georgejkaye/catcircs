open Value
open Printer

type 'a io_cmp = { input : 'a signal array; outputs : 'a signal array array }

module type VInOut = sig
  type v

  val all_equal : v io_cmp -> bool
  val string_of_io_cmp : v io_cmp -> string
  val string_of_io_cmp_list : v io_cmp list -> string
end

module ExtendInOut (V : Value) : VInOut with type v = V.v = struct
  type v = V.v

  module VString = ExtendString (V)

  let all_equal io =
    List.fold_left
      (fun acc i ->
        let l = VString.string_of_signal_array io.outputs.(i) in
        let r = VString.string_of_signal_array io.outputs.(0) in
        acc && String.equal l r)
      true
      (List.init (Array.length io.outputs - 1) (fun i -> i + 1))

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
