open Values
module IntSet = Set.Make (Int)

type 'a binop = { symbol : string; fn : 'a -> 'a -> 'a }
type 'a unop = { symbol : string; fn : 'a -> 'a }

type 'a expression =
  | Constant of 'a
  | Variable of int
  | UnOp of 'a unop * 'a expression
  | BinOp of 'a binop * 'a expression * 'a expression

module type VExp = sig
  type v

  val string_of_expression : v expression -> string
end

module ExtendVExp (V : V) : VExp with type v := V.v = struct
  let rec string_of_expression exp =
    match exp with
    | Constant v -> V.string_of_value v
    | Variable i -> "v" ^ string_of_int i
    | UnOp (op, e) -> op.symbol ^ string_of_expression e
    | BinOp (op, l, r) ->
        string_of_expression l ^ " " ^ op.symbol ^ string_of_expression r
end

let get_vars exp =
  let rec get_vars' acc exp =
    match exp with
    | Constant _ -> acc
    | Variable i -> IntSet.add i acc
    | UnOp (_, e) -> get_vars' acc e
    | BinOp (_, l, r) ->
        let acc' = get_vars' acc l in
        get_vars' acc' r
  in
  get_vars' IntSet.empty exp

let rec eval assgs exp =
  match exp with
  | Constant a -> a
  | Variable i -> Hashtbl.find assgs i
  | UnOp (op, e) -> op.fn (eval assgs e)
  | BinOp (op, l, r) -> op.fn (eval assgs l) (eval assgs r)
