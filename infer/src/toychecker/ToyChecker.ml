(* ToyChecker for practice *)

open! IStd
module F = Format
module L = Logging

let checker {IntraproceduralAnalysis.proc_desc; err_log} =
  print_endline "Hi ToyChecker";
