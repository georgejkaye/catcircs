open Sig
open Expression

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

module Belnap : Sig = struct
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

module BelnapExpression = ExtendExp (Belnap)
