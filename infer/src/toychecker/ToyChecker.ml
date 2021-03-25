(* ToyChecker for practice *)

open! IStd
module F = Format
module L = Logging

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = ToyCheckerDomain

  type analysis_data = Procdesc.t

  let exec_instr astate proc_desc _ = function
    | Sil.Store {typ= {desc = Tptr _}; e2= rhs_exp} ->
        print_endline "ToyChecker transfer functions";
        astate
    | Sil.Load {id= lhs_id; e= rhs_exp} ->
        print_endline "ToyChecker transfer functions";
        astate
    | _ -> astate

  let pp_session_name _node fmt = F.pp_print_string fmt "ToyChecker"
end

module CFG = ProcCfg.Normal

module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions (CFG))
  
let checker ({IntraproceduralAnalysis.proc_desc; err_log} as analysis_data) =
  print_endline "Hi ToyChecker";
