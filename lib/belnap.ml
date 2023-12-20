open Values

type belnap = Bottom | False | True | Top

module Belnap : V = struct
  type v = belnap

  let string_of_value = function
    | Bottom -> "⊥"
    | False -> "f"
    | True -> "t"
    | Top -> "⊤"

  let values = [ Bottom; False; True; Top ]
end

(* let belnap_values =
   List.fold_left
     (fun acc cur -> Set.add cur acc)
     BelnapSet.empty
     [ Bottom; False; True; Top ] *)

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
