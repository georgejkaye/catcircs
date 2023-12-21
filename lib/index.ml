open Sig
open Circuits
open Inout
open Expression

module VPFuncs (V : Sig) = struct
  include ExtendString (V)
  include ExtendEnum (V)
  include ExtendSet (V)
  include ExtendInOut (V)
  include ExtendCircuit (V)
  include ExtendExp (V)
end
