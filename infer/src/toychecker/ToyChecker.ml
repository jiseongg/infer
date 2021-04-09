(* ToyChecker for practice *)

open! IStd
module F = Format
module L = Logging

module CFG = ProcCfg.NormalOneInstrPerNode

module Dom = ToyCheckerDomain

module TransferFunctions = struct
  module CFG = ProcCfg.NormalOneInstrPerNode
  module Domain = Dom

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

module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions)

let report_issues {IntraproceduralAnalysis.proc_desc; err_log} cfg invariant_map = 
  let report_null_deref pre_state instr =
    match instr with 
    | Sil.Store {e1= Exp.Var id; e2; loc} -> (
        match Dom.find_opt (Var.of_id id) pre_state with
        | Some Dom.Nullable ->
            let message = F.asprintf "Null dereference on %a" Ident.pp id in
            Reporting.log_issue proc_desc err_log ~loc ToyChecker IssueType.toy_checker_err message
        | _ -> ()
      )
    | Sil.Store {e1= Exp.Lvar pvar; e2; loc} -> (
        match Dom.find_opt (Var.of_pvar pvar) pre_state with
        | Some Dom.Nullable ->
            let message = F.asprintf "Null dereference on %a"
                (fun fmt -> Pvar.pp Pp.text fmt) pvar in
            Reporting.log_issue proc_desc err_log ~loc ToyChecker IssueType.toy_checker_err message
        | _ -> ()
      )
    | Sil.Load {id; e= Exp.Var _id; loc} -> (
        match Dom.find_opt (Var.of_id _id) pre_state with
        | Some Dom.Nullable ->
            let message = F.asprintf "Null dereference on %a" Ident.pp _id in
            Reporting.log_issue proc_desc err_log ~loc ToyChecker IssueType.toy_checker_err message
        | _ -> ()
      )
    | Sil.Load {id; e= Exp.Lvar pvar; loc} -> (
        match Dom.find_opt (Var.of_pvar pvar) pre_state with
        | Some Dom.Nullable ->
            let message = F.asprintf "Null dereference on %a"
                (fun fmt -> Pvar.pp Pp.text fmt) pvar in
            Reporting.log_issue proc_desc err_log ~loc ToyChecker IssueType.toy_checker_err message
        | _ -> ()
      )
    | _ -> ()
  in
  let report_on_node node = 
    let node_id = CFG.Node.id node in
    let instrs = CFG.instrs node in
    Instrs.iter instrs
      ~f:(fun instr ->
          match Analyzer.extract_pre node_id invariant_map with
          | Some pre_state ->
              report_null_deref pre_state instr
          | None ->
              () )
  in
  Container.iter cfg ~fold:CFG.fold_nodes ~f:report_on_node

let checker ({IntraproceduralAnalysis.proc_desc; err_log} as analysis_data) =
  let pvars = Procdesc.get_pvar_formals proc_desc in
  let initialize_pvar astate = function
    | (pvar, {Typ.desc= Tptr _}) ->
        Dom.add (Var.of_pvar pvar) Dom.Nullable astate
    | (pvar, _) ->
        Dom.add (Var.of_pvar pvar) Dom.NonNull astate
  in
  let initial =
    List.fold_left ~f:initialize_pvar ~init:Dom.initial pvars 
  in
  let cfg = CFG.from_pdesc proc_desc in
  let invariant_map = Analyzer.exec_pdesc
      ~do_narrowing:false ~initial analysis_data proc_desc in
  report_issues analysis_data cfg invariant_map

