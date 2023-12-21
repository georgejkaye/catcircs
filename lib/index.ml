open Values
open Circuits
open Inout
open Expression

module VFuncs (V : V) = struct
  include ExtendString (V)
  include ExtendEnum (V)
  include ExtendSet (V)
  include ExtendInOut (V)
  include ExtendCircuit (V)
  include ExtendExp (V)
end
