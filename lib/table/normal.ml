open Table
open Expression
open Value

module type MonotoneNF = sig
  type v
  type p

  val y_unit : v
  val y_high : v
  val y_op : p
  val x_unit : v
  val x_high : v
  val x_op : p
end

module type MonotoneNFMaker = sig
  type v
  type p

  val table_to_nf : (v array * v) array -> (v, p) expression
  val tidy_nf_exp : (v, p) expression -> (v, p) expression
end

module ExtendMonotoneNF (V : Value) (NF : MonotoneNF with type v = V.v) = struct
  module VString = ExtendString (V)

  let to_nf table =
    let rows = table.table in
    Array.fold_left
      (fun acc (ins, ous) ->
        if Core.phys_equal NF.y_high ous then
          Op
            ( NF.y_op,
              [|
                acc;
                (let _, exp =
                   Array.fold_left
                     (fun (i, acc) cur ->
                       let next =
                         if Core.phys_equal NF.x_high cur then
                           Op (NF.x_op, [| acc; Variable i |], 0)
                         else acc
                       in
                       (i + 1, next))
                     (0, Constant NF.x_unit) ins
                 in
                 exp);
              |],
              0 )
        else acc)
      (Constant NF.y_unit) rows

  let rec tidy_nf exp =
    match exp with
    | Op (op, args, i) -> (
        let tidied_args = Array.map tidy_nf args in
        let unit_val =
          if Core.phys_equal op NF.x_op then Some NF.x_unit
          else if Core.phys_equal op NF.y_op then Some NF.y_unit
          else None
        in
        match unit_val with
        | None -> Op (op, tidied_args, i)
        | Some v -> (
            match tidied_args.(0) with
            | Constant w when Core.phys_equal v w ->
                let args_length = Array.length tidied_args in
                if Core.phys_equal args_length 2 then tidied_args.(1)
                else
                  let other_args = Array.sub tidied_args 1 (args_length - 1) in
                  Op (op, other_args, i)
            | _ -> Op (op, tidied_args, i)))
    | _ -> exp
end
