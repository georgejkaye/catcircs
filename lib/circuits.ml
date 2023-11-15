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
  {
    fn = value_fn_to_signal_fn fn;
    arity = List.init ar (fun _ -> 1);
    coarity = List.init coar (fun _ -> 1);
  }

let make_circuit_signals fn ar coar = { fn; arity = ar; coarity = coar }

type io_cmp = { input : signal list; lhs : signal list; rhs : signal list }

let string_of_io io =
  Values.string_of_signal_list io.input
  ^ " || "
  ^ Values.string_of_signal_list io.lhs
  ^ " | "
  ^ Values.string_of_signal_list io.rhs
  ^ if io.lhs = io.rhs then "  O" else "  X "

let string_of_io_cmp_list =
  string_of_list string_of_io ~delim:"\n" ~opening:"" ~closing:""

let compare c1 c2 =
  enumerate_inputs c1.arity
  |> List.map (fun ss ->
         let lhs = c1.fn ss in
         let rhs = c2.fn ss in
         { input = ss; lhs; rhs })

let get_errors ios =
  List.rev
    (List.fold_left
       (fun acc io -> if io.lhs = io.rhs then acc else io :: acc)
       [] ios)

let string_of_comparison c1 c2 =
  let comparison = compare c1 c2 in
  let row_length =
    List.fold_left max 0
      (List.map (fun cmp -> string_of_io cmp |> String.length) comparison)
  in
  let equals_lines = String.init row_length (fun _ -> '=') in
  let table = string_of_io_cmp_list comparison in
  let errors = get_errors comparison in
  let error_text =
    if List.length errors == 0 then "All clear!"
    else "Errors:\n" ^ string_of_io_cmp_list errors
  in
  equals_lines ^ "\n" ^ table ^ "\n" ^ equals_lines ^ "\n" ^ error_text
