open Values
open Primitive
module IntSet = Set.Make (Int)

type 'a binop = { symbol : string; bin : 'a -> 'a -> 'a }

let make_binop sym fn = { symbol = sym; bin = fn }

type 'a unop = { symbol : string; un : 'a -> 'a }

let make_unop sym fn = { symbol = sym; un = fn }

type ('v, 'p) expression =
  | Constant of 'v
  | Variable of int
  | Op of 'p * ('v, 'p) expression array * int

module type VPExp = sig
  type v
  type p

  val string_of_expression : (v, p) expression -> string
  val get_vars : (v, p) expression -> IntSet.t
  val eval : (int, v) Hashtbl.t -> (v, p) expression -> v
end

module ExtendExp (V : V) (P : P with type v := V.v) :
  VPExp with type v := V.v and type p := P.p = struct
  let rec string_of_expression exp =
    match exp with
    | Constant v -> V.string_of_value v
    | Variable i -> "v" ^ string_of_int i
    | Op (op, exps, i) ->
        let exp_strings = Core.Array.map ~f:string_of_expression exps in
        P.applied_string_of_primitive op exp_strings i

  let get_vars exp =
    let rec get_vars' acc exp =
      match exp with
      | Constant _ -> acc
      | Variable i -> IntSet.add i acc
      | Op (_, exps, _) ->
          Core.Array.fold ~init:acc
            ~f:(fun acc cur -> IntSet.union acc (get_vars' acc cur))
            exps
    in
    get_vars' IntSet.empty exp

  let rec eval assgs exp =
    match exp with
    | Constant a -> a
    | Variable i -> Hashtbl.find assgs i
    | Op (op, exps, i) ->
        let eval_exps = Core.Array.map ~f:(eval assgs) exps in
        (P.fn_of_primitive op eval_exps).(i)
end
