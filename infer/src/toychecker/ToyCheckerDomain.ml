(* ToyChecker for practice *)

open! IStd
module F = Format
module L = Logging

(* Simple domain data *)
type nullable = Nullable | NonNull

(* Key for invariant map *)
module PVar = struct
  type t = Pvar.t [@@deriving compare]
  
  let pp = Pvar.pp Pp.text
end

module DomainData = struct
  type t = nullable

  let leq ~lhs ~rhs =
    match (lhs, rhs) with
    | (NonNull, _)
    | (_, Nullable) -> true
    | _ -> false

  let join a b =
    match (a, b) with
    | (Nullable, _)
    | (_, Nullable) -> Nullable
    | _ -> NonNull

  let widen ~prev ~next ~num_iters = join prev next

  let pp fmt domain_data =
    let str_domain_data =
      match domain_data with
      | NonNull -> "NonNull"
      | Nullable -> "Nullable"
    in
    F.fprintf fmt "%s" str_domain_data
end

include AbstractDomain.Map (PVar) (DomainData)

let initial = empty

