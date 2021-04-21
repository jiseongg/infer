(* Abstract Location *)

open! IStd

module F = Format
module L = Logging

type t = 
  | SingleElem of Pvar.t
  | Top
[@@deriving compare, equal]

let to_string loc =
  match loc with
  | SingleElem pvar -> Pvar.to_string pvar
  | Top -> "Top"

let pp fmt loc = 
  let str_loc = to_string loc in
  F.fprintf fmt "%s" str_loc

let is_top = function Top -> true | _ -> false

let top = Top

let join a b = if equal a b then a else Top

let widen ~prev ~next ~num_iters = join prev next

let leq ~lhs ~rhs = if equal lhs rhs then true else false

let get_pvar = function
  | SingleElem pvar -> pvar
  | _ -> raise (Failure "Unknown Location")

