(* Abstract store *)

open! IStd

include AbstractDomain.Map (Ident) (CachedVal)

let initial = empty
