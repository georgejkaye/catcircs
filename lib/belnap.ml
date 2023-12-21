open Values
open Expression

type belnap = Bottom | False | True | Top
[@@deriving enumerate, sexp, compare]

module Belnap : V = struct
  type v = belnap

  let all_of_v = all_of_belnap
  let sexp_of_v = sexp_of_belnap
  let v_of_sexp = belnap_of_sexp
  let compare_v = compare_belnap

  let string_of_value = function
    | Bottom -> "⊥"
    | False -> "f"
    | True -> "t"
    | Top -> "⊤"
end

module BelnapSet = ExtendSet (Belnap)

let string_of_belnap_bools = function
  | Bottom -> "00"
  | False -> "01"
  | True -> "10"
  | Top -> "11"

let fork v = (v, v)

let join_fn x y =
  match (x, y) with
  | Bottom, w -> w
  | v, Bottom -> v
  | Top, _ -> Top
  | _, Top -> Top
  | True, False -> Top
  | False, True -> Top
  | True, True -> True
  | False, False -> False

let join_op = { symbol = "⊔"; fn = join_fn }

let and_fn x y =
  match (x, y) with
  | False, _ -> False
  | _, False -> False
  | True, w -> w
  | v, True -> v
  | Bottom, Bottom -> Bottom
  | Bottom, Top -> False
  | Top, Bottom -> False
  | Top, Top -> Top

let or_fn x y =
  match (x, y) with
  | True, _ -> True
  | _, True -> True
  | False, w -> w
  | v, False -> v
  | Bottom, Bottom -> Bottom
  | Bottom, Top -> True
  | Top, Bottom -> True
  | Top, Top -> Top

let not_fn = function
  | Bottom -> Bottom
  | True -> False
  | False -> True
  | Top -> Top
