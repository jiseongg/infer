(* ToyChecker for practice *)

open! IStd
module F = Format
module L = Logging

module CFG = ProcCfg.NormalOneInstrPerNode

module Domain = AbsState

module TransferFunctions = struct
  module CFG = ProcCfg.NormalOneInstrPerNode
  module Domain = Domain

  type analysis_data = IntraproceduralAnalysis.t 

  let rec eval exp (heap, store) =
    match exp with
    | Exp.Var id ->
        let (_, abs_val) = AbsStore.find id store in
        abs_val
    | Exp.Const (Const.Cint i) ->
        if (IntLit.isnull i) || (IntLit.iszero i)
        then AbsVal.Nullable else AbsVal.Top
    | Exp.Cast (typ, e) ->
        AbsVal.type_cast typ (eval e (heap, store))
    | Exp.Lvar pvar ->
        if AbsHeap.mem (AbsLoc.SingleElem pvar) heap
        then AbsVal.NonNull else AbsVal.Bottom
    | _ -> AbsVal.Top

  let rec prune_exp exp (heap, store) =
    match exp with
    | Exp.BinOp (Binop.Eq, Exp.Var id, e)
    | Exp.BinOp (Binop.Eq, e, Exp.Var id)
    | Exp.UnOp (Unop.LNot, (Exp.BinOp (Binop.Ne, Exp.Var id, e)), _)
    | Exp.UnOp (Unop.LNot, (Exp.BinOp (Binop.Ne, e, Exp.Var id)), _) ->
        let value = eval e (heap, store) in
        let abs_loc =
          match AbsStore.find id store with
          | (Some abs_loc', _) -> abs_loc'
          | (None, _) -> raise (Failure "unknown value")
        in
        (AbsHeap.add abs_loc value heap, store)
    | Exp.BinOp (Binop.Ne, Exp.Var id, e)
    | Exp.BinOp (Binop.Ne, e, Exp.Var id)
    | Exp.UnOp (Unop.LNot, (Exp.BinOp (Binop.Eq, Exp.Var id, e)), _)
    | Exp.UnOp (Unop.LNot, (Exp.BinOp (Binop.Eq, e, Exp.Var id)), _) ->
        let value = eval e (heap, store) in
        let abs_loc = 
          match AbsStore.find id store with
          | (Some abs_loc', _) -> abs_loc'
          | (None, _) -> raise (Failure "unknown value")
        in
        (AbsHeap.add abs_loc (AbsVal.lnot value) heap, store)
    | _ -> (heap, store)

  let exec_instr (heap, store) proc_desc _ _ = fun instr ->
    match instr with
    | Sil.Load {id; e= Exp.Lvar pvar} ->
        let abs_loc = AbsLoc.SingleElem pvar in
        let abs_val = AbsHeap.find abs_loc heap in
        let store' = AbsStore.add id (Some abs_loc, abs_val) store in
        (heap, store')
    | Sil.Store {e1= Exp.Lvar pvar1; e2= Exp.Lvar pvar2} ->
        let heap' =
          if AbsHeap.mem (AbsLoc.SingleElem pvar2) heap
          then AbsHeap.add (AbsLoc.SingleElem pvar1) NonNull heap
          else AbsHeap.add (AbsLoc.SingleElem pvar1) Bottom heap
        in
        (heap', store)
    | Sil.Store {e1= Exp.Lvar pvar; typ; e2= Exp.Var id} ->
        let (abs_loc, abs_val) =
          match AbsStore.find id store with
          | None, abs_val' -> ((AbsLoc.SingleElem pvar), abs_val')
          | Some abs_loc', abs_val' -> (abs_loc', abs_val')
        in
        let heap' = AbsHeap.add abs_loc abs_val heap in
        let store' = AbsStore.add id (Some abs_loc, abs_val) store in
        (heap', store')
    | Sil.Call ((id, typ), _, args, _, _) ->
        let return_val =
          match typ with
          | Typ.{desc= Tptr _} -> AbsVal.Nullable
          | _ -> AbsVal.NonPtr
        in
        let store' = AbsStore.add id (None, return_val) store in
        (heap, store')
    | Sil.Prune (exp, _, _, _) ->
        prune_exp exp (heap, store)
    | Sil.Metadata (Sil.ExitScope (vars, _)) -> 
        let delete_var (heap', store') = function
          | Var.ProgramVar pvar ->
              (AbsHeap.remove (AbsLoc.SingleElem pvar) heap, store')
          | Var.LogicalVar id ->
              (heap', AbsStore.remove id store')
        in
        List.fold_left ~f:delete_var ~init:(heap, store) vars
    | Sil.Metadata (Sil.VariableLifetimeBegins
                      (pvar, Typ.{desc= Tptr _}, _)) ->
        let heap' = AbsHeap.add (AbsLoc.SingleElem pvar) AbsVal.Nullable heap in
        (heap', store)
    | Sil.Metadata (Sil.VariableLifetimeBegins
                      (pvar, _, _)) ->
        let heap' = AbsHeap.add (AbsLoc.SingleElem pvar) AbsVal.Bottom heap in
        (heap', store)
    | _ ->
        (heap, store)

  let pp_session_name _node fmt = F.pp_print_string fmt "ToyChecker"
end

module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions)

let report_issues {IntraproceduralAnalysis.proc_desc; err_log} cfg invariant_map = 
  let report_null_deref (pre_heap, pre_store) instr =
    match instr with 
    | Sil.Store {e1= Exp.Var _id; loc}
    | Sil.Load {e= Exp.Var _id; loc} -> (
        let (abs_loc, abs_val) =
          match AbsStore.find _id pre_store with
          | (Some abs_loc', abs_val') -> (abs_loc', abs_val')
          | _ -> raise (Failure "unknown value")
        in
        let pvar = AbsLoc.get_pvar abs_loc in
        match AbsHeap.find_opt abs_loc pre_heap with
        | Some AbsVal.Nullable ->
            let message = F.asprintf "Potential null dereference: *%a could be Null" (Pvar.pp Pp.text) pvar in
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
  let initialize_formals heap = function
    | (pvar, {Typ.desc= Tptr _}) ->
        AbsHeap.add (AbsLoc.SingleElem pvar) AbsVal.Nullable heap
    | (pvar, _) ->
        AbsHeap.add (AbsLoc.SingleElem pvar) AbsVal.NonPtr heap
  in
  let locals = Procdesc.get_locals proc_desc in
  let initialize_locals heap = function
    | ProcAttributes.{name; typ} ->
        let pvar = Pvar.mk name (Procdesc.get_proc_name proc_desc) in 
        AbsHeap.add (AbsLoc.SingleElem pvar) AbsVal.Bottom heap
  in
  let initial_heap =
    List.fold_left locals ~f:initialize_locals ~init:(
      List.fold_left formals ~f:initialize_formals ~init:AbsHeap.initial)
  in
  let initial_store = AbsStore.initial in
  let initial = (initial_heap, initial_store) in
  let cfg = CFG.from_pdesc proc_desc in
  let invariant_map = Analyzer.exec_pdesc
      ~do_narrowing:false ~initial analysis_data proc_desc in
  report_issues analysis_data cfg invariant_map

