open Normal
open Value
open Primitive
open Table
open Expression

type boolval = T | F [@@deriving enumerate, sexp, compare]
type boolgate = Band | Bor | Bnot [@@deriving enumerate, sexp, compare]

module BoolVal : Value with type v = boolval = struct
  type v = boolval

  let string_of_value = function T -> "T" | F -> "F"
  let all_of_v = all_of_boolval
  let sexp_of_v = sexp_of_boolval
  let compare_v = compare_boolval
  let v_of_sexp = boolval_of_sexp
end

let band_fn x y = match (x, y) with F, _ -> F | _, F -> F | T, T -> T
let bor_fn x y = match (x, y) with T, _ -> T | _, T -> T | F, F -> F
let bnot_fn x = match x with T -> F | F -> T

module BoolGate : Primitive with type v = boolval and type p = boolgate = struct
  type v = boolval
  type p = boolgate

  let all_of_p = all_of_boolgate
  let sexp_of_p = sexp_of_boolgate
  let p_of_sexp = boolgate_of_sexp
  let compare_p = compare_boolgate

  let string_of_primitive = function
    | Band -> "BAND"
    | Bor -> "BOR"
    | Bnot -> "BNOT"

  let arity_of_primitive = function Band -> 2 | Bor -> 2 | Bnot -> 1
  let coarity_of_primitive _ = 1

  let applied_string_of_primitive p args _ =
    let symbol = match p with Band -> "∧" | Bor -> "∨" | Bnot -> "¬" in
    match p with
    | Bnot ->
        let arg = args.(0) in
        let lbracket, rbracket =
          if String.contains arg ' ' then ("(", ")") else ("", "")
        in
        symbol ^ lbracket ^ arg ^ rbracket
    | _ ->
        let lhs = args.(0) in
        let rhs = args.(1) in
        lhs ^ " " ^ symbol ^ " " ^ rhs

  let fn_of_primitive p args =
    let output =
      match p with
      | Band -> band_fn args.(0) args.(1)
      | Bor -> bor_fn args.(0) args.(1)
      | Bnot -> bnot_fn args.(0)
    in
    [| output |]
end

module BoolTable = ExtendTable (BoolVal)
module BoolExp = ExtendExp (BoolVal) (BoolGate)

module DNF : MonotoneNF with type v = boolval and type p = boolgate = struct
  type v = boolval
  type p = boolgate

  let y_unit = F
  let y_high = T
  let y_op = Bor
  let x_unit = T
  let x_high = T
  let x_op = Band
end

module DNFMaker = ExtendMonotoneNF (BoolVal) (DNF)
