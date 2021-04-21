(* Abstract Location *)
open! IStd

type t = 
  | SingleElem of Pvar.t
  | Top
[@@deriving compare, equal]

include AbstractDomain.WithTop with type t := t

val to_string : t -> string

val get_pvar : t -> Pvar.t

