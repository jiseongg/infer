(* Abstract heap *)

open! IStd

module F = Format
module L = Logging

include AbstractDomain.Map (AbsLoc) (AbsVal)

let initial = empty
