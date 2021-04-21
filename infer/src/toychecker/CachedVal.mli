(* Cached value in AbsStore *)

open! IStd

type t = AbsLoc.t option * AbsVal.t

include AbstractDomain.S with type t := t
