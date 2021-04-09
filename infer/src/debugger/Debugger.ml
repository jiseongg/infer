(* Debugger for practice *)

open! IStd
module F = Format

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = DebuggerDomain

  type analysis_data = IntraproceduralAnalysis.t

  let exec_instr astate _ _ _ = fun instr ->
    match instr with
    | Sil.Store _ ->
        F.eprintf "%s\n" "Store: ";
        Sil.pp_instr ~print_types:false
          Pp.text F.err_formatter instr;
        F.eprintf "%s\n\n" "";
        astate
    | Sil.Load _ ->
        F.eprintf "%s\n" "Load: ";
        Sil.pp_instr ~print_types:false
          Pp.text F.err_formatter instr;
        F.eprintf "%s\n\n" "";
        astate
    | Sil.Prune _ ->
        F.eprintf "%s\n" "Prune: ";
        Sil.pp_instr ~print_types:false
          Pp.text F.err_formatter instr;
        F.eprintf "%s\n\n" "";
        astate
    | Sil.Call _ ->
        F.eprintf "%s\n" "Call: ";
        Sil.pp_instr ~print_types:false
          Pp.text F.err_formatter instr;
        F.eprintf "%s\n\n" "";
        astate
    | Sil.Metadata _ ->
        F.eprintf "%s\n" "Metadata: ";
        Sil.pp_instr ~print_types:false
          Pp.text F.err_formatter instr;
        F.eprintf "%s\n\n" "";
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "Debugger"
end

module CFG = ProcCfg.Normal

module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions (CFG))
  
let checker ({IntraproceduralAnalysis.proc_desc} as analysis_data) =
  let initial = DebuggerDomain.initial in
  match Analyzer.compute_post analysis_data ~initial proc_desc with
    | _ ->
        ()

