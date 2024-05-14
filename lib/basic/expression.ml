open Value
open Primitive
module IntSet = Set.Make (Int)

type ('v, 'p) expression =
  | Constant of 'v
  | Variable of int
  | Op of 'p * ('v, 'p) expression array * int

type ('v, 'p) nodetype =
  | CNode of 'v
  | VNode of int
  | ONode of 'p * int
  | OutNode

type ('v, 'p) node = { id : string; ntype : ('v, 'p) nodetype }

module type VPExp = sig
  type v
  type p

  val string_of_expression : (v, p) expression -> string
  val get_vars : (v, p) expression -> IntSet.t
  val eval : (int, v) Hashtbl.t -> (v, p) expression -> v
  val graph_of_expression : (v, p) expression -> string
end

module ExtendExp (V : Value) (P : Primitive with type v = V.v) :
  VPExp with type v = V.v and type p = P.p = struct
  type v = V.v
  type p = P.p

  module VString = ExtendString (V)

  let rec string_of_expression exp =
    match exp with
    | Constant v -> V.string_of_value v
    | Variable i -> "v" ^ string_of_int i
    | Op (op, exps, i) ->
        let exp_strings = Core.Array.map ~f:string_of_expression exps in
        "(" ^ P.applied_string_of_primitive op exp_strings i ^ ")"

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

  let graph_of_expression exp =
    let rec generate_graph_elements cur parent next_id nodes edges max_var =
      match cur with
      | Constant v ->
          let nodeid = "c" ^ string_of_int next_id in
          let newnode = { id = nodeid; ntype = CNode v } in
          (newnode :: nodes, (nodeid, parent) :: edges, next_id + 1, max_var)
      | Variable i ->
          ( nodes,
            ("var" ^ string_of_int i, parent) :: edges,
            next_id,
            max i max_var )
      | Op (op, exps, i) ->
          let nodeid = "op" ^ string_of_int next_id in
          let newnode = { id = nodeid; ntype = ONode (op, i) } in
          let nodes, edges, next_id, max_var =
            Array.fold_left
              (fun (nodes, edges, next_id, max_var) cur ->
                generate_graph_elements cur nodeid (next_id + 1) nodes edges
                  max_var)
              (nodes, edges, next_id + 1, max_var)
              exps
          in
          let newedge = (nodeid, parent) in
          (newnode :: nodes, newedge :: edges, next_id, max_var)
    in
    let outputid = "o" in
    let outputnode = { id = outputid; ntype = OutNode } in
    let nodes, edges, _, max_var =
      generate_graph_elements exp outputid 0 [ outputnode ] [] 0
    in
    let nodes =
      List.fold_left
        (fun acc i -> { id = "var" ^ string_of_int i; ntype = VNode i } :: acc)
        nodes
        (List.init max_var (fun i -> i))
    in
    let nodes = { id = "o"; ntype = OutNode } :: nodes in
    let nodes_to_dot =
      List.fold_left
        (fun acc cur ->
          let label =
            match cur.ntype with
            | CNode v -> V.string_of_value v
            | VNode i -> "v" ^ string_of_int i
            | ONode (op, _) -> P.string_of_primitive op
            | OutNode -> "output"
          in
          acc ^ "\n" ^ cur.id ^ "[label=" ^ label ^ "]")
        ""
    in
    let node_string = nodes_to_dot nodes in
    let edges_to_dot =
      List.fold_left
        (fun acc (source, target) -> acc ^ "\n" ^ source ^ "->" ^ target)
        ""
    in
    let edge_string = edges_to_dot edges in
    "digraph G {" ^ node_string ^ edge_string ^ "\n}"
end

let setup_assgs assgs =
  let ctx = Hashtbl.create (Array.length assgs) in
  let _ =
    Array.fold_left
      (fun i cur ->
        Hashtbl.add ctx i cur;
        i + 1)
      0 assgs
  in
  ctx
