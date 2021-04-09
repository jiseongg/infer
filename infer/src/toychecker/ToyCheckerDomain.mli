(* ToyChecker for practice *)

open! IStd

type nullable = Nullable | NonNull | NonPtr | Bottom | Top
[@@deriving equal]   

module DomainData : sig
  include AbstractDomain.WithTop with type t = nullable

  include AbstractDomain.WithBottom with type t := t

  val binop : t -> t -> t

  val neg : t -> t

  val lneg : t -> t

  val type_cast: Typ.t -> t -> t
end

include module type of AbstractDomain.Map (Var) (DomainData)

(* overwrite map operations with key of Var.t *)
val add : Var.t -> nullable -> t -> t

val remove : Var.t -> t -> t

val find : Var.t -> t -> nullable

val mem : Var.t -> t -> bool

val initial : t
