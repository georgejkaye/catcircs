module type Primitive = sig
  type v
  type p [@@deriving enumerate, sexp, compare]

  val string_of_primitive : p -> string
  val arity_of_primitive : p -> int
  val coarity_of_primitive : p -> int
  val applied_string_of_primitive : p -> string array -> int -> string
  val fn_of_primitive : p -> v array -> v array
end
