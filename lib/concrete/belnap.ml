open Value
open Lattice
open Primitive
open Expression
open Circuits

type belnap = Bottom | False | True | Top
[@@deriving enumerate, sexp, compare]

type gate = And | Or | Not | Join [@@deriving enumerate, sexp, compare]

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

module BelnapValue : Value with type v = belnap = struct
  type v = belnap

  let string_of_value = function
    | Bottom -> "⊥"
    | False -> "f"
    | True -> "t"
    | Top -> "⊤"

  let all_of_v = all_of_belnap
  let sexp_of_v = sexp_of_belnap
  let v_of_sexp = belnap_of_sexp
  let compare_v = compare_belnap
end

module BelnapLattice : Lattice with type v := belnap = struct
  let join v w =
    match (v, w) with
    | Bottom, w -> w
    | v, Bottom -> v
    | True, True -> True
    | False, False -> False
    | _, _ -> Top

  let meet v w =
    match (v, w) with
    | Top, w -> w
    | v, Top -> v
    | True, True -> True
    | False, False -> False
    | _, _ -> Bottom
end

module BelnapGate : Primitive with type v = belnap and type p = gate = struct
  type v = belnap
  type p = gate

  let all_of_p = all_of_gate
  let sexp_of_p = sexp_of_gate
  let p_of_sexp = gate_of_sexp
  let compare_p = compare_gate

  let string_of_primitive = function
    | And -> "AND"
    | Or -> "OR"
    | Not -> "NOT"
    | Join -> "JOIN"

  let arity_of_primitive = function And -> 2 | Or -> 2 | Not -> 1 | Join -> 2
  let coarity_of_primitive _ = 1

  let applied_string_of_primitive p ss _ =
    let symbol =
      match p with And -> "∧" | Or -> "∨" | Not -> "¬" | Join -> "⊔"
    in
    match p with
    | Not ->
        let arg = ss.(0) in
        let lbracket, rbracket =
          if String.contains arg ' ' then ("(", ")") else ("", "")
        in
        symbol ^ lbracket ^ arg ^ rbracket
    | _ ->
        let lhs = ss.(0) in
        let rhs = ss.(1) in
        lhs ^ " " ^ symbol ^ " " ^ rhs

  let fn_of_primitive p vs =
    let output =
      match p with
      | And -> and_fn vs.(0) vs.(1)
      | Or -> or_fn vs.(0) vs.(1)
      | Not -> not_fn vs.(0)
      | Join -> join_fn vs.(0) vs.(1)
    in
    [| output |]
end

module BelnapString = ExtendString (BelnapValue)
module BelnapExpression = ExtendExp (BelnapValue) (BelnapGate)
module BelnapCircuit = ExtendCircuit (BelnapValue)
module BelnapInOut = Inout.ExtendInOut (BelnapValue)

let explode x =
  let f_l = and_fn Bottom x in
  let f_r = not_fn (or_fn Bottom x) in
  let t_l = not_fn (and_fn Bottom x) in
  let t_r = or_fn Bottom x in
  [| f_l; f_r; t_l; t_r |]

let explodes_intersperse xs =
  let each_explode = Array.map explode xs in
  let len = Array.length xs in
  Array.init (len * 4) (fun i -> each_explode.(i mod len).(i / len))

let explodes_concat xs =
  let each_explode = Array.map explode xs in
  let len = Array.length xs in
  Array.init (len * 4) (fun i -> each_explode.(i / 4).(i mod 4))

let explodes = explodes_concat
let reexplode l r = [| l; not_fn r; not_fn l; r |]

let wrap_explode fn xs =
  let exps = explodes xs in
  let outs = fn exps in
  [| join_fn outs.(0) outs.(3) |]

let exp_concat = Array.concat
let exp_id_fn xs = reexplode xs.(0) xs.(3)
let exp_not_fn xs = reexplode xs.(1) xs.(2)

let exp_and_fn xs =
  let f_and = and_fn xs.(0) xs.(4) in
  let t_and = and_fn xs.(3) xs.(7) in
  reexplode f_and t_and

let exp_or_fn xs =
  let f_or = or_fn xs.(0) xs.(4) in
  let t_or = or_fn xs.(3) xs.(7) in
  reexplode f_or t_or
