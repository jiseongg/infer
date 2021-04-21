(* Abstract heap *)

open! IStd

include AbstractDomain.MapS with type key = AbsLoc.t and type value = AbsVal.t  

val initial : t
