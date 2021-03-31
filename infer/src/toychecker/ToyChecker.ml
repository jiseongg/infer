(* ToyChecker for practice *)

open! IStd
module F = Format
module L = Logging

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = ToyCheckerDomain

  type analysis_data = IntraproceduralAnalysis.t

  let exec_instr astate proc_desc _ = fun instr ->
    match instr with
    | Sil.Store _ ->
        astate
    | Sil.Load _ ->
        astate
    | Sil.Prune _ ->
        astate
    | Sil.Call ((_, _), _, args, _, _) ->
        let add_call_with_ptrs astate' = function
          | (Exp.Lvar pvar, _) ->
              Domain.add pvar Domain.Nullable astate'
          | _ -> astate'
        in
        List.fold_left ~f:add_call_with_ptrs ~init:astate args
    | Sil.Metadata (Sil.ExitScope (vars, _)) ->
        let delete_pvars astate' = function
          | Var.ProgramVar pvar ->
              Domain.remove pvar astate'
          | _ -> astate'
        in
        List.fold_left ~f:delete_pvars ~init:astate vars
    | Sil.Metadata (Sil.VariableLifetimeBegins 
                      (pvar, {Typ.desc= Tptr _}, _)) ->
        Domain.add pvar Domain.Nullable astate
    | _ ->
        astate

  let pp_session_name _node fmt = F.pp_print_string fmt "ToyChecker"
end

module CFG = ProcCfg.Normal

module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions (CFG))
  
let checker ({IntraproceduralAnalysis.proc_desc; err_log} as analysis_data) =
  let pvars = Procdesc.get_pvar_formals proc_desc in
  let initialize_pvar astate = function
    | (pvar, {Typ.desc= Tptr _}) ->
        ToyCheckerDomain.add pvar ToyCheckerDomain.Nullable astate
    | (pvar, _) ->
        ToyCheckerDomain.add pvar ToyCheckerDomain.NonNull astate
  in
  let initial =
    List.fold_left ~f:initialize_pvar ~init:ToyCheckerDomain.initial pvars 
  in
  let result =  Analyzer.compute_post analysis_data ~initial proc_desc in
  match result with
  | _ ->
      ()
