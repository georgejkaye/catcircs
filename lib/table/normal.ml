open Table
open Expression

module type MonotoneNF = sig
  type v
  type p

  val row_unit : v
  val row_high : v
  val row_op : p
  val col_unit : v
  val col_high : v
  val col_op : p
end

module type MonotoneNFMaker = sig
  type v
  type p

  val table_to_nf : (v array * v) array -> (v, p) expression
end

module ExtendMonotoneNF (NF : MonotoneNF) = struct
  let to_nf table =
    let rows = table.table in
    Array.fold_left
      (fun acc (ins, ous) ->
        if Core.phys_equal NF.row_high ous then
          Op
            ( NF.row_op,
              [|
                acc;
                (let _, exp =
                   Array.fold_left
                     (fun (i, acc) cur ->
                       let next =
                         if Core.phys_equal NF.col_high cur then
                           Op (NF.col_op, [| acc; Variable i |], 0)
                         else acc
                       in
                       (i + 1, next))
                     (0, Constant NF.col_unit) ins
                 in
                 exp);
              |],
              0 )
        else acc)
      (Constant NF.row_unit) rows
end
