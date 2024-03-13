open Circuits
open Value
open Inout

let rec drop n xs =
  if n == 0 then xs
  else
    match xs with
    | [] -> failwith "drop: not enough list"
    | _ :: xs -> drop (n - 1) xs

let take n xs =
  let rec take' acc n xs =
    if n == 0 then List.rev acc
    else
      match xs with
      | [] -> failwith "take: not enough list"
      | x :: xs -> take' (x :: acc) (n - 1) xs
  in
  take' [] n xs

let split n xs =
  let rec split' acc n xs =
    if n == 0 then (List.rev acc, xs)
    else
      match xs with
      | [] -> failwith "split: not enough list"
      | x :: xs -> split' (x :: acc) (n - 1) xs
  in
  split' [] n xs

module type VBisim = sig
  type v

  val get_agreeing_states : v circuit -> v circuit -> int -> v signal array list
end

module ExtendBisim (V : Value) : VBisim with type v := V.v = struct
  module VEnum = ExtendEnum (V)
  module VCircuit = ExtendCircuit (V)
  module VInOut = ExtendInOut (V)
  module VString = ExtendString (V)

  let get_agreeing_states f g state_size =
    let state_widths, input_widths = split state_size f.arity in
    let all_inputs = VEnum.enumerate_inputs input_widths in
    let all_states = VEnum.enumerate_inputs state_widths in
    let output_indices =
      List.mapi
        (fun i _ -> i + state_size)
        (List.init
           (List.length f.coarity - state_size)
           (fun i -> i + state_size))
    in
    List.filter
      (fun state ->
        let complete_inputs =
          List.map (fun input -> Array.concat [ state; input ]) all_inputs
        in
        VCircuit.compare_with f g complete_inputs
        |> VInOut.some_of_list_equal output_indices)
      all_states
end
