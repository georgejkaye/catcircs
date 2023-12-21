(* open Belnap.Signature

   type ('a, 'b) mealy = {
     inputs : int;
     outputs : int;
     states : int list;
     fn : int -> 'a -> int * 'b;
     initial : int;
   }

   let delay_mealy_value_state_assg = function
     | Bottom -> 0
     | True -> 1
     | False -> 2
     | Top -> 3

   let delay_mealy_state_value_assg = function
     | 0 -> Bottom
     | 1 -> True
     | 2 -> False
     | 3 -> Top
     | _ -> failwith "bad state"

   let delay_mealy_fn s v =
     (delay_mealy_value_state_assg v, delay_mealy_state_value_assg s)

   let delay_mealy =
     {
       inputs = 1;
       outputs = 1;
       states = [ 0; 1; 2; 3 ];
       fn = delay_mealy_fn;
       initial = delay_mealy_value_state_assg Bottom;
     }

   let simulate_from_state mm = mm.fn
   let simulate_from_initial mm = simulate_from_state mm mm.initial

   let simulate_many_from_state mm s vs =
     let s, trace =
       List.fold_left
         (fun (s, acc) cur ->
           let t, w = simulate_from_state mm s cur in
           (t, w :: acc))
         (s, []) vs
     in
     (s, List.rev trace)

   let simulate_many_from_initial mm vs = simulate_many_from_state mm mm.initial vs *)
