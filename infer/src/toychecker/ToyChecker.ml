(* ToyChecker for practice *)

open! IStd
module F = Format
module L = Logging

(* interval domain here is supported by only non-negative endpoint *)

type endpoint = Inf | Int of int

let ep_order lhs rhs =
  match (lhs, rhs) with
  | (_, Inf) -> true
  | (Inf, _) -> raise (Failure "Inf < _: Undefined operations for NonNegEP")
  | (Int lep, Int rep) ->
      if lep < 0 || rep < 0
      then raise (Failure "NonNegEP only support positive integers")
      else lep <= rep

type interval = Itv of endpoint * endpoint | Bot

type t = interval
 
let leq ~lhs ~rhs = 
  match (lhs, rhs) with
  | (Bot, _) -> true
  | (_, Bot) -> false
  | (Itv (lep1, rep1), Itv(lep2, rep2)) ->
      if ep_order lep2 lep1 && ep_order rep1 rep2
      then true
      else false

let join _a _b =
  match (_a, _b) with
  | (Bot, _)
  | (_, Bot) -> Bot
  | (Itv (lep1, rep1), Itv(lep2, rep2)) ->
      let _lep = if ep_order lep1 lep2 then lep2 else lep1 in
      let _rep = if ep_order rep1 rep2 then rep1 else rep2 in
      if ep_order _lep _rep then Itv(_lep, _rep) else Bot

let widen ~prev:_ ~next:_ ~num_iters:_ = assert false

let pp fmt astate = assert false

let initial = Itv (Int 0, Inf)

let checker {IntraproceduralAnalysis.proc_desc; err_log} =
  print_endline "Hi ToyChecker";
