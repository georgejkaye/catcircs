open Values

let fork v = (v, v)

let join_fn = function
  | Bottom, w -> w
  | v, Bottom -> v
  | Top, _ -> Top
  | _, Top -> Top
  | True, False -> Top
  | False, True -> Top
  | True, True -> True
  | False, False -> False

let and_fn = function
  | False, _ -> False
  | _, False -> False
  | True, w -> w
  | v, True -> v
  | Bottom, Bottom -> Bottom
  | Bottom, Top -> False
  | Top, Bottom -> False
  | Top, Top -> Top

let or_fn = function
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

let pair (op1, op2) (x, y) = (op1 x, op2 y)

type circuit = {
  fn : signal list -> signal list;
  arity : int list;
  coarity : int list;
}

let value_fn_to_signal_fn fn ss =
  List.map (fun s -> List.hd s.values) ss
  |> fn
  |> List.map (fun v -> { values = [ v ] })

let make_circuit_values fn ar coar =
  { fn = value_fn_to_signal_fn fn; arity = ar; coarity = coar }

let make_circuit_signals fn ar coar = { fn; arity = ar; coarity = coar }

type io_cmp = { input : signal list; lhs : signal list; rhs : signal list }

let compare c1 c2 =
  enumerate_inputs c1.arity
  |> List.map (fun ss ->
         let lhs = c1.fn ss in
         let rhs = c2.fn ss in
         { input = ss; lhs; rhs })
