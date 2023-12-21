open Value
open Base
open Printer
open Inout

type 'a circuit = {
  fn : 'a signal array -> 'a signal array;
  arity : int list;
  coarity : int list;
}

module type VCirc = sig
  type v

  val compare : v circuit -> v circuit -> v io_cmp list
  val string_of_outputs : v circuit -> string
  val string_of_comparison : v circuit -> v circuit -> string
  val make_circuit_values : (v array -> v array) -> int -> int -> v circuit

  val make_circuit_signals :
    (v signal array -> v signal array) -> int list -> int list -> v circuit
end

module ExtendCircuit (V : Value) : VCirc with type v := V.v = struct
  module VString = ExtendString (V)
  module VEnum = ExtendEnum (V)
  module VInOut = ExtendInOut (V)

  let compare c1 c2 =
    VEnum.enumerate_inputs c1.arity
    |> List.map ~f:(fun ss ->
           let lhs = c1.fn ss in
           let rhs = c2.fn ss in
           { input = ss; lhs; rhs })

  let string_of_outputs c =
    let inputs = VEnum.enumerate_inputs c.arity in
    let outputs = List.map ~f:(fun ss -> c.fn ss) inputs in
    let rows =
      List.map2
        ~f:(fun ins outs ->
          VString.string_of_signal_array ins
          ^ " | "
          ^ VString.string_of_signal_array outs)
        inputs outputs
    in
    match rows with
    | Ok xs ->
        string_of_list (fun x -> x) ~opening:"" ~closing:"" ~delim:"\n" xs
    | Unequal_lengths -> failwith "never"

  let get_errors ios =
    List.rev
      (List.fold_left ~init:[]
         ~f:(fun acc io ->
           if Array.equal phys_equal io.lhs io.rhs then acc else io :: acc)
         ios)

  let string_of_comparison c1 c2 =
    let comparison = compare c1 c2 in
    let row_length =
      List.fold_left ~init:0 ~f:max
        (List.map
           ~f:(fun cmp -> VInOut.string_of_io cmp |> String.length)
           comparison)
    in
    let equals_lines = String.init row_length ~f:(fun _ -> '=') in
    let table = VInOut.string_of_io_cmp_list comparison in
    let errors = get_errors comparison in
    let error_text =
      if phys_equal (List.length errors) 0 then "All clear!"
      else "Errors:\n" ^ VInOut.string_of_io_cmp_list errors
    in
    equals_lines ^ "\n" ^ table ^ "\n" ^ equals_lines ^ "\n" ^ error_text

  let value_fn_to_signal_fn fn ss =
    Array.map ~f:(fun s -> s.values.(0)) ss
    |> fn
    |> Array.map ~f:(fun v -> { values = [| v |] })

  let make_circuit_values fn ar coar =
    {
      fn = value_fn_to_signal_fn fn;
      arity = List.init ar ~f:(fun _ -> 1);
      coarity = List.init coar ~f:(fun _ -> 1);
    }

  let make_circuit_signals fn ar coar = { fn; arity = ar; coarity = coar }
end
