(* ToyChecker for practice *)

open! IStd
module F = Format
module L = Logging

(* Simple domain data *)
type nullable = Nullable | NonNull | NonPtr | Bottom | Top
[@@deriving equal]   

module DomainData = struct
  type t = nullable

  let bottom = Bottom

  let is_bottom = equal_nullable Bottom

  let top = Top

  let is_top = equal_nullable Top

  let leq ~lhs ~rhs =
    match (lhs, rhs) with
    | (Bottom, _) | (_, Top) -> true
    | (Top, _) | (_, Bottom) -> false
    | (NonPtr, NonPtr) -> true
    | (NonPtr, _) -> false
    | (NonNull, NonPtr) -> false
    | (NonNull, _) -> true
    | (Nullable, Nullable) -> true
    | _ -> false

  let join a b =
    match (a, b) with
    | (Top, _) | (_, Top) -> Top
    | (Bottom, v) | (v, Bottom) -> v
    | (NonPtr, NonPtr) -> NonPtr
    | (NonPtr, _) -> Top
    | (NonNull, NonNull) -> NonNull
    | (NonNull, Nullable) -> Nullable
    | (NonNull, _) -> Top
    | (Nullable, NonPtr) -> Top
    | (Nullable, _) -> Nullable

  let widen ~prev ~next ~num_iters = join prev next

  let binop a b =
    match (a, b) with 
    | (Top, _) | (_, Top) -> Top
    | (Bottom, _) | (_, Bottom) -> Bottom
    | (NonPtr, NonPtr) -> NonPtr
    | _ -> Top

  let neg a =
    match a with
    | Top -> Top
    | Bottom -> Bottom
    | NonPtr -> NonPtr
    | _ -> Top

  let lneg a =
    match a with
    | Top -> Top
    | Bottom -> Bottom
    | NonPtr -> NonPtr
    | Nullable -> NonNull
    | NonNull -> Nullable

  let type_cast typ a =
    match (typ, a) with
    | _, Bottom -> Bottom
    | _, Top -> Top
    | Typ.{desc= Tptr _}, Nullable -> Nullable
    | Typ.{desc= Tptr _}, NonNull -> NonNull
    | Typ.{desc= Tptr _}, NonPtr -> Bottom
    | _, NonPtr | _, Nullable | _, NonNull -> NonPtr

  let pp fmt domain_data =
    let str_domain_data =
      match domain_data with
      | NonNull -> "NonNull"
      | Nullable -> "Nullable"
      | NonPtr -> "NonPtr"
      | Bottom -> "Bottom"
      | Top -> "Top"
    in
    F.fprintf fmt "%s" str_domain_data
end

include AbstractDomain.Map (Var) (DomainData)

let initial = empty

