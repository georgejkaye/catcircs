open Values
open Base
open Printer

type 'a io_cmp = {
  input : 'a signal array;
  lhs : 'a signal array;
  rhs : 'a signal array;
}

module type VInOut = sig
  type v

  val string_of_io : v io_cmp -> string
  val string_of_io_cmp_list : v io_cmp list -> string
end

module ExtendInOut (V : V) : VInOut with type v := V.v = struct
  module VString = ExtendString (V)

  let string_of_io io =
    VString.string_of_signal_array io.input
    ^ " || "
    ^ VString.string_of_signal_array io.lhs
    ^ " | "
    ^ VString.string_of_signal_array io.rhs
    ^ if Array.equal phys_equal io.lhs io.rhs then "  O" else "  X "

  let string_of_io_cmp_list =
    string_of_list string_of_io ~delim:"\n" ~opening:"" ~closing:""
end
