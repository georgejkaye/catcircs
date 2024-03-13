open Value

module type Lattice = sig
  type v

  val join : v -> v -> v
  val meet : v -> v -> v
end

module type LatticeDerived = sig
  type v

  val leq : v -> v -> bool
  val leq_signal : v signal -> v signal -> bool
  val leq_inputs : v signal array -> v signal array -> bool
  val geq : v -> v -> bool
  val geq_signal : v signal -> v signal -> bool
  val geq_inputs : v signal array -> v signal array -> bool
  val join_signal : v signal -> v signal -> v signal
  val join_inputs : v signal array -> v signal array -> v signal array
  val bot : v
  val top : v
  val all_less_than : v signal array -> v signal array list
end

module ExtendLattice (V : Value) (L : Lattice with type v = V.v) :
  LatticeDerived with type v := V.v = struct
  module Enum = ExtendEnum (V)
  module VString = ExtendString (V)

  let leq a b = Core.phys_equal b (L.join a b)

  let leq_signal xs ys =
    Array.fold_left
      (fun acc cur -> acc && cur)
      true
      (Array.map2 (fun x y -> leq x y) xs.values ys.values)

  let leq_inputs xss yss =
    Array.fold_left
      (fun acc cur -> acc && cur)
      true
      (Array.map2 (fun xs ys -> leq_signal xs ys) xss yss)

  let geq a b = (not (leq a b)) && not (Core.phys_equal a b)

  let geq_signal xs ys =
    Array.fold_left
      (fun acc cur -> acc && cur)
      true
      (Array.map2 (fun x y -> geq x y) xs.values ys.values)

  let geq_inputs xss yss =
    Array.fold_left
      (fun acc cur -> acc && cur)
      true
      (Array.map2 (fun xs ys -> geq_signal xs ys) xss yss)

  let join_signal xs ys = { values = Array.map2 L.join xs.values ys.values }
  let join_inputs xss yss = Array.map2 join_signal xss yss

  let bot =
    let values = V.all_of_v in
    List.fold_left
      (fun acc cur -> if leq acc cur then acc else cur)
      (List.hd values) (List.tl values)

  let top =
    let values = V.all_of_v in
    List.fold_left
      (fun acc cur -> if geq acc cur then acc else cur)
      (List.hd values) (List.tl values)

  let all_less_than vs =
    let string_of_inputs = VString.string_of_signal_array vs in
    let widths = Enum.widths_from_inputs vs in
    let possibles = Enum.enumerate_inputs widths in
    List.filter
      (fun inp ->
        leq_inputs inp vs
        && not
             (String.equal
                (VString.string_of_signal_array inp)
                string_of_inputs))
      possibles
end
