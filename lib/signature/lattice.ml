open Value

module type Lattice = sig
  type v

  val leq : v -> v -> bool
  val geq : v -> v -> bool
  val bot : v
  val top : v
end

module ExtendLattice (V : Value) (L : Lattice with type v = V.v) = struct
  let leq a b = Core.phys_equal b (V.join a b)
  let geq a b = (not (leq a b)) && not (Core.phys_equal a b)

  let bot =
    let values = V.all_of_v in
    List.fold_left
      (fun acc cur -> if L.leq acc cur then acc else cur)
      (List.hd values) (List.tl values)

  let top =
    let values = V.all_of_v in
    List.fold_left
      (fun acc cur -> if L.geq acc cur then acc else cur)
      (List.hd values) (List.tl values)
end
