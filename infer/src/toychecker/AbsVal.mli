(* AbstractValue *)

open! IStd

type nullable = Nullable | NonNull | NonPtr | Bottom | Top
[@@deriving equal]   

include AbstractDomain.WithTop with type t = nullable

include AbstractDomain.WithBottom with type t := t

val binop : t -> t -> t

val neg : t -> t

val lnot : t -> t

val type_cast: Typ.t -> t -> t
