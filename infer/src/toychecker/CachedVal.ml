(* Cached value in AbsStore *)

open! IStd

module F = Format
module L = Logging

type t = AbsLoc.t option * AbsVal.t

let leq ~lhs ~rhs = assert false

let join _a _b = assert false

let widen ~prev ~next ~num_iters = assert false

let pp fmt (_loc, value) =
  let _loc_str =
    match _loc with
    | None -> "None"
    | Some loc -> AbsLoc.to_string loc
  in
  F.fprintf fmt "{loc: %s; val: %a}"
    _loc_str (fun abs_val' fmt' -> AbsVal.pp abs_val' fmt') value

