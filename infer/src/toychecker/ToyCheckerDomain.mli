(* ToyChecker for practice *)

open! IStd

type nullable = Nullable | NonNull

module DomainData : AbstractDomain.S with type t = nullable

include module type of AbstractDomain.Map (Var) (DomainData)

(* overwrite map operations with key of Var.t *)
val add : Var.t -> nullable -> t -> t

val remove : Var.t -> t -> t

val find : Var.t -> t -> nullable

val mem : Var.t -> t -> bool

val initial : t
