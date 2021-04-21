(* Abstract store *)

open! IStd

include AbstractDomain.MapS with type key = Ident.t and type value = CachedVal.t

val initial : t
