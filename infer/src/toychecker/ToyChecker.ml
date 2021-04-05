(* ToyChecker for practice *)

open! IStd
module F = Format
module L = Logging

module TransferFunctions = struct
  module CFG = ProcCfg.NormalOneInstrPerNode
  module Domain = ToyCheckerDomain

  type analysis_data = IntraproceduralAnalysis.t
  
  let of_id = Var.of_id
  
  let of_pvar = Var.of_pvar

  let exec_instr astate proc_desc _ = fun instr ->
    match instr with
    | Sil.Store {e1= Exp.Var id; typ= Typ.{desc= Tptr _}} ->
        Domain.add (of_id id) Domain.Nullable astate
    | Sil.Store {e1= Exp.Lvar pvar; typ= Typ.{desc= Tptr _}} ->
        Domain.add (of_pvar pvar) Domain.Nullable astate
    | Sil.Call ((id, typ), _, args, _, _) ->
        let new_astate = 
          match typ with
          | Typ.{desc= Tptr _} ->
              Domain.add (of_id id) Domain.Nullable astate
          | _ -> astate
        in
        let add_call_with_ptrs astate' = function
          | Exp.Lvar pvar, Typ.{desc= Tptr ({desc= Tptr _}, _)} ->
              Domain.add (of_pvar pvar) Domain.Nullable astate'
          | _ -> astate
        in
        List.fold_left ~f:add_call_with_ptrs ~init:new_astate args
    | Sil.Metadata (Sil.ExitScope (vars, _)) ->
        let delete_var astate' var = Domain.remove var astate' in
        List.fold_left ~f:delete_var ~init:astate vars
    | Sil.Metadata (Sil.VariableLifetimeBegins
                      (pvar, {Typ.desc= Tptr _}, _)) ->
        Domain.add (of_pvar pvar) Domain.Nullable astate
    | _ ->
        astate

  let pp_session_name _node fmt = F.pp_print_string fmt "ToyChecker"
end

module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

let report_null_deref {IntraproceduralAnalysis.proc_desc; err_log} invariant_map =
  let _report_null_deref node instr = 
    let node_id = Procdesc.Node.get_id in
    let instr_idx = ProcCfg.InstrNode.id
    match Analyzer.extract_pre node invariant_map with
    | Some pre_state -> (
        match instr with
        | Sil.Store {e1 = Exp.Var id; e2} -> (
            match ToyCheckerDomain.find (Var.of_id id) pre_state with
            | ToyCheckerDomain.Nullable ->
                let loc = Procdesc.get_loc proc_desc in
                let message = F.asprintf "Null dereference on %a" Ident.pp id in
                Reporting.log_issue proc_desc err_log loc
                  ToyChecker IssueType.toy_checker_err message
            | _ -> ()
          )
        | _ -> ()
      )
    | _ -> ()
  in
  Procdesc.iter_instrs _report_null_deref procdesc

let checker ({IntraproceduralAnalysis.proc_desc; err_log} as analysis_data) =
  let pvars = Procdesc.get_pvar_formals proc_desc in
  let initialize_pvar astate = function
    | (pvar, {Typ.desc= Tptr _}) ->
        ToyCheckerDomain.add (Var.of_pvar pvar) ToyCheckerDomain.Nullable astate
    | (pvar, _) ->
        ToyCheckerDomain.add (Var.of_pvar pvar) ToyCheckerDomain.NonNull astate
  in
  let initial =
    List.fold_left ~f:initialize_pvar ~init:ToyCheckerDomain.initial pvars 
  in
  let invariant_map = Analyzer.exec_cfg
      ~do_narrowing:false ~initial:initial proc_desc analysis_data in
  report_null_deref analysis_data invariant_map

