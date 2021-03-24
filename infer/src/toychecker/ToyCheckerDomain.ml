(* ToyChecker for practice *)

open! IStd
module F = Format
module L = Logging

(* interval domain here is supported by only non-negative endpoint *)

type endpoint = Inf | Int of int

let string_of_ep ep =
  match ep with
  | Inf -> "Inf"
  | Int x -> string_of_int x

let ep_order lhs rhs =
  match (lhs, rhs) with
  | (_, Inf) -> true
  | (Inf, _) -> raise (Failure "Inf < _: Undefined operations for NonNegEP")
  | (Int lep, Int rep) ->
      if lep < 0 || rep < 0
      then raise (Failure "NonNegEP only support positive integers")
      else lep <= rep

module Interval = struct
  type t = endpoint * endpoint

  let leq ~lhs ~rhs = 
    match (lhs, rhs) with
    | (lep1, rep1), (lep2, rep2) ->
        if ep_order lep2 lep1 && ep_order rep1 rep2
        then true
        else false

  let join _a _b =
    match (_a, _b) with
    | (lep1, rep1), (lep2, rep2) ->
        let _lep = if ep_order lep1 lep2 then lep2 else lep1 in
        let _rep = if ep_order rep1 rep2 then rep1 else rep2 in
        (_lep, _rep)
  
  let widen ~prev ~next ~num_iters = join prev next
  
  let pp fmt itv = 
    match itv with
    | l, h -> F.fprintf fmt "[%s, %s]"
                      (string_of_ep l) (string_of_ep h)
end

include AbstractDomain.BottomLifted (Interval)
open AbstractDomain.Types

let initial = NonBottom (Int 0, Inf)

