open Value
open Primitive
open Circuits
open Inout
open Expression

module VPFuncs (V : Value) (P : Primitive with type v = V.v) = struct
  include ExtendString (V)
  include ExtendEnum (V)
  include ExtendSet (V)
  include ExtendInOut (V)
  include ExtendCircuit (V)
  include ExtendExp (V) (P)
end
