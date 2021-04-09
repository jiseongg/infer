(* ToyChecker for practice *)

open! IStd
module F = Format
module L = Logging

module CFG = ProcCfg.NormalOneInstrPerNode

module Domain = ToyCheckerDomain

module DomainData = ToyCheckerDomain.DomainData
  
module TransferFunctions = struct
  module CFG = ProcCfg.NormalOneInstrPerNode
  module Domain = Domain

  type analysis_data = IntraproceduralAnalysis.t 
  
  let of_id = Var.of_id
  
  let of_pvar = Var.of_pvar

  let is_pvar_in pvar astate =
    Domain.mem (of_pvar pvar) astate

  let is_id_in id astate =
    Domain.mem (of_id id) astate

  let get_with_pvar pvar astate =
    Domain.find (of_pvar pvar) astate

  let get_with_id id astate =
    Domain.find (of_id id) astate

  let add_id id domain_value astate =
    Domain.add (of_id id) domain_value astate

  let add_pvar pvar domain_value astate =
    Domain.add (of_pvar pvar) domain_value astate

  let rec eval exp astate = 
    match exp with
    | Exp.Var id -> get_with_id id astate
    | Exp.UnOp (Unop.Neg, e, _) ->
        DomainData.neg (eval e astate)
    | Exp.UnOp (Unop.LNot, e, _) ->
        DomainData.lneg (eval e astate)
    | Exp.BinOp (_, e1, e2) ->
        DomainData.binop (eval e1 astate) (eval e2 astate)
    | Exp.Const (Const.Cint i) ->
        if (IntLit.isnull i) || (IntLit.iszero i)
        then Domain.Nullable else Domain.Top
    | Exp.Cast (typ, e) -> 
        DomainData.type_cast typ (eval e astate)
    | Exp.Lvar pvar ->
        if is_pvar_in pvar astate
        then Domain.NonNull else Domain.Bottom
    | Exp.Lfield (_, _, Typ.{desc= Tptr _}) ->
        Domain.Nullable
    | Exp.Lindex (_, _) -> Domain.Top
    | _ -> Domain.Top

  let rec prune_exp exp astate =
    match exp with
    | Exp.BinOp (Binop.Eq, Exp.Var id, e)
    | Exp.BinOp (Binop.Eq, e, Exp.Var id)
    | Exp.UnOp (Unop.LNot, (Exp.BinOp (Binop.Ne, Exp.Var id, e)), _)
    | Exp.UnOp (Unop.LNot, (Exp.BinOp (Binop.Ne, e, Exp.Var id)), _) ->
        let value = eval e astate in
        add_id id value astate
    | Exp.BinOp (Binop.Ne, Exp.Var id, e)
    | Exp.BinOp (Binop.Ne, e, Exp.Var id)
    | Exp.UnOp (Unop.LNot, (Exp.BinOp (Binop.Eq, Exp.Var id, e)), _)
    | Exp.UnOp (Unop.LNot, (Exp.BinOp (Binop.Eq, e, Exp.Var id)), _) ->
        let value = eval e astate in
        add_id id (DomainData.lneg value) astate
    | _ -> astate

  let exec_instr astate proc_desc _ _ = fun instr ->
    match instr with
    | Sil.Store {e1= Exp.Lvar pvar; typ; e2} ->
        let dom_val = eval (Exp.Cast (typ, e2)) astate in
        add_pvar pvar dom_val astate
    | Sil.Load {id; e; typ=Typ.{desc= Tptr _}} ->
        let dom_val =  match eval e astate with
          | Domain.NonPtr | Domain.Nullable | Domain.Bottom ->
              Domain.Bottom
          | Domain.Top -> Domain.Top
          | Domain.NonNull -> Domain.Nullable
        in
        add_id id dom_val astate
    | Sil.Load {id; e; _} ->
        let dom_val =  match eval e astate with
          | Domain.NonPtr | Domain.Nullable | Domain.Bottom ->
              Domain.Bottom
          | Domain.Top -> Domain.Top
          | Domain.NonNull -> Domain.NonPtr
        in
        add_id id dom_val astate
    | Sil.Call ((id, typ), _, args, _, _) ->
        let return_val = 
          match typ with
          | Typ.{desc= Tptr _} -> Domain.Nullable
          | _ -> Domain.NonPtr
        in
        let new_astate = add_id id return_val astate in
        let add_call_with_ptrs astate' = function
          | Exp.Lvar pvar, Typ.{desc= Tptr ({desc= Tptr _}, _)} ->
              add_pvar pvar Domain.Nullable astate'
          | _ -> astate'
        in
        List.fold_left ~f:add_call_with_ptrs ~init:new_astate args
    | Sil.Prune (exp, _, _, _) ->
        prune_exp exp astate
    | Sil.Metadata (Sil.ExitScope (vars, _)) ->
        let delete_var astate' var = Domain.remove var astate' in
        List.fold_left ~f:delete_var ~init:astate vars
    | Sil.Metadata (Sil.VariableLifetimeBegins
                      (pvar, _, _)) ->
        add_pvar pvar Domain.Bottom astate
    | _ ->
        astate

  let pp_session_name _node fmt = F.pp_print_string fmt "ToyChecker"
end

module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions)

let report_issues {IntraproceduralAnalysis.proc_desc; err_log} cfg invariant_map = 
  let report_null_deref pre_state instr =
    match instr with 
    | Sil.Store {e1= Exp.Var id; e2; loc} -> (
        match Domain.find_opt (Var.of_id id) pre_state with
        | Some Domain.Nullable ->
            let message = F.asprintf "Null dereference on %a" Ident.pp id in
            Reporting.log_issue proc_desc err_log ~loc ToyChecker IssueType.toy_checker_err message
        | _ -> ()
      )
    | Sil.Load {id; e= Exp.Var _id; loc} -> (
        match Domain.find_opt (Var.of_id _id) pre_state with
        | Some Domain.Nullable ->
            let message = F.asprintf "Null dereference on %a" Ident.pp _id in
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
  let formals = Procdesc.get_pvar_formals proc_desc in
  let initialize_formals astate = function
    | (pvar, {Typ.desc= Tptr _}) ->
        Domain.add (Var.of_pvar pvar) Domain.Nullable astate
    | (pvar, _) ->
        Domain.add (Var.of_pvar pvar) Domain.NonPtr astate
  in
  let locals = Procdesc.get_locals proc_desc in
  let initialize_locals astate = function
    | ProcAttributes.{name;} ->
        let pvar = Pvar.mk name (Procdesc.get_proc_name proc_desc) in 
        Domain.add (Var.of_pvar pvar) Domain.Bottom astate
  in
  let initial =
    List.fold_left locals ~f:initialize_locals ~init:(
      List.fold_left formals ~f:initialize_formals ~init:Domain.initial)
  in
  let cfg = CFG.from_pdesc proc_desc in
  let invariant_map = Analyzer.exec_pdesc
      ~do_narrowing:false ~initial analysis_data proc_desc in
  report_issues analysis_data cfg invariant_map

