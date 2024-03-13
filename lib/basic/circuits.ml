open Value
open Base
open Printer
open Inout
open Lattice

type 'a circuit = {
  fn : 'a signal array -> 'a signal array;
  arity : int list;
  coarity : int list;
}

let get_value_fn circ vs =
  Array.map ~f:(fun v -> { values = [| v |] }) vs
  |> circ.fn
  |> Array.map ~f:(fun v -> v.values.(0))

module type VCirc = sig
  type v

  val test : v circuit -> v io_cmp list
  val compare : v circuit -> v circuit -> v io_cmp list

  val compare_with :
    v circuit -> v circuit -> v signal array list -> v io_cmp list

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

  let test c1 =
    VEnum.enumerate_inputs c1.arity
    |> List.map ~f:(fun ss ->
           let out = c1.fn ss in
           { input = ss; outputs = [| out |] })

  let compare_with c1 c2 =
    List.map ~f:(fun ss ->
        let lhs = c1.fn ss in
        let rhs = c2.fn ss in
        { input = ss; outputs = [| lhs; rhs |] })

  let compare c1 c2 = VEnum.enumerate_inputs c1.arity |> compare_with c1 c2

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
           if
             Array.fold
               ~f:(fun acc cur ->
                 acc && Array.equal phys_equal io.outputs.(0) cur)
               ~init:true io.outputs
           then acc
           else io :: acc)
         ios)

  let string_of_comparison c1 c2 =
    let comparison = compare c1 c2 in
    let row_length =
      List.fold_left ~init:0 ~f:max
        (List.map
           ~f:(fun cmp -> VInOut.string_of_io_cmp cmp |> String.length)
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

module type VLatticeCirc = sig
  type v

  val is_monotone : v circuit -> bool

  val monotone_completion :
    (v signal array -> v signal array option) ->
    int array ->
    v signal array ->
    v signal array
end

module ExtendLatticeCircuit (V : Value) (L : Lattice with type v = V.v) :
  VLatticeCirc with type v = V.v = struct
  type v = V.v

  module VEnum = ExtendEnum (V)
  module VCircuit = ExtendCircuit (V)
  module VLattice = ExtendLattice (V) (L)

  let is_monotone circ =
    let inputs = VEnum.enumerate_inputs circ.arity in
    let is_monotone_for_fixed_input fixed_input =
      let fixed_output = circ.fn fixed_input in
      List.fold_left
        ~f:(fun acc cur ->
          acc
          &&
          if not (VLattice.leq_inputs fixed_input cur) then true
          else
            let output2 = circ.fn cur in
            VLattice.leq_inputs fixed_output output2)
        ~init:true inputs
    in
    List.fold_left
      ~f:(fun acc1 cur1 -> acc1 && is_monotone_for_fixed_input cur1)
      ~init:true inputs

  let rec monotone_completion f outputs vs =
    match f vs with
    | Some ws -> ws
    | None ->
        if input_is_all VLattice.bot vs then
          make_signal_array_of_all VLattice.bot outputs
        else
          let lowers = VLattice.all_less_than vs in
          List.fold_left
            ~f:(fun acc cur ->
              let lower_f = monotone_completion f outputs cur in
              VLattice.join_inputs acc lower_f)
            ~init:(make_signal_array_of_all VLattice.bot outputs)
            lowers
end
