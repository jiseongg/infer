(* ToyChecker for practice *)

open! IStd

type nullable = Nullable | NonNull

module DomainData : AbstractDomain.S with type t = nullable

module PVar : PrettyPrintable.PrintableOrderedType

include module type of AbstractDomain.Map (PVar) (DomainData)

val add : Pvar.t -> nullable -> t -> t

val remove : Pvar.t -> t -> t

val mem : Pvar.t -> t -> bool

val initial : t
