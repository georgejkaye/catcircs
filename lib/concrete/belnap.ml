open Value
open Lattice
open Primitive
open Expression
open Circuits

type belnap = Bottom | False | True | Top
[@@deriving enumerate, sexp, compare]

type gate = And | Or | Not [@@deriving enumerate, sexp, compare]

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

let explode x =
  let f_l = and_fn Bottom x in
  let f_r = not_fn (or_fn Bottom x) in
  let t_l = not_fn (and_fn Bottom x) in
  let t_r = or_fn Bottom x in
  (f_l, f_r, t_l, t_r)

let explodes xs =
  let each_explode = Array.map explode xs in
  let len = Array.length xs in
  Array.init (len * 4) (fun i ->
      let a, b, c, d = each_explode.(i mod len) in
      let index = i / len in
      if index == 0 then a
      else if index == 1 then b
      else if index == 2 then c
      else d)

let reexplode l r = [| l; not_fn r; not_fn l; r |]

let wrap_explode fn xs =
  let exps = explodes xs in
  let outs = fn exps in
  [| join_fn outs.(0) outs.(3) |]

let exp_concat xs ys =
  let fl1, fr1, tl1, tr1 = xs in
  let fl2, fr2, tl2, tr2 = ys in
  (fl1, fr1, tl1, tr1, fl2, fr2, tl2, tr2)

let exp_id_fn xs =
  let fl, _, _, tr = xs in
  reexplode fl tr

let exp_not_fn xs = reexplode xs.(1) xs.(2)

let exp_and_fn xs =
  let f_and = and_fn xs.(0) xs.(1) in
  let t_and = and_fn xs.(6) xs.(7) in
  reexplode f_and t_and

let exp_or_fn xs =
  let f_or = or_fn xs.(0) xs.(1) in
  let t_or = or_fn xs.(6) xs.(7) in
  reexplode f_or t_or

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
  let string_of_primitive = function And -> "AND" | Or -> "OR" | Not -> "NOT"
  let arity_of_primitive = function And -> 2 | Or -> 2 | Not -> 1
  let coarity_of_primitive _ = 1

  let applied_string_of_primitive p ss _ =
    let symbol = match p with And -> "∧" | Or -> "∨" | Not -> "¬" in
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
    in
    [| output |]
end

module BelnapString = ExtendString (BelnapValue)
module BelnapExpression = ExtendExp (BelnapValue) (BelnapGate)
module BelnapCircuit = ExtendCircuit (BelnapValue)
module BelnapInOut = Inout.ExtendInOut (BelnapValue)
