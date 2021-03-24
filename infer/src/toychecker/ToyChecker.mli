(* ToyChecker for practice *)

open! IStd

include AbstractDomain.S

val initial : t

val checker : IntraproceduralAnalysis.t -> unit
