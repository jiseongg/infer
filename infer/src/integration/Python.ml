(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

type kind = Bytecode of {files: string list} | Files of {prog: string; args: string list}

module Error = struct
  type t = FFI of FFI.Error.t | IR of PyIR.Error.t

  let format_error file error =
    let log level =
      match (level : L.error) with
      | InternalError ->
          L.internal_error
      | ExternalError ->
          L.external_error
      | UserError ->
          L.user_error
    in
    match error with
    | FFI (level, err) ->
        log level "[python:%s][-1] %a@\n" file FFI.Error.pp_kind err
    | IR (level, loc, err) ->
        log level "[python:%s][%a] %a@\n" file PyIR.Location.pp loc PyIR.Error.pp_kind err


  let ffi x = FFI x

  let ir x = IR x
end

let process_file ~is_binary file =
  let open IResult.Let_syntax in
  let _sourcefile = Textual.SourceFile.create file in
  let* code = Result.map_error ~f:Error.ffi @@ FFI.from_file ~is_binary file in
  let* ir = Result.map_error ~f:Error.ir @@ PyIR.mk ~debug:false code in
  if Config.run_python_interpreter then PyIRExec.run ir ;
  Ok ()


let _dump_file ~next_to_source pyc module_ =
  let filename =
    if next_to_source then
      let filename = SourceFile.create pyc in
      Filename.chop_extension (SourceFile.to_abs_path filename) ^ ".sil"
    else
      let textual_filename = TextualSil.to_filename pyc in
      Filename.temp_file ~in_dir:(ResultsDir.get_path Temporary) textual_filename "sil"
  in
  TextualSil.dump_module ~filename module_


let capture_file ~is_binary file = process_file ~is_binary file

let capture_files ~is_binary files =
  let n_files = List.length files in
  let child_action, child_prologue, child_epilogue =
    let child_tenv = Tenv.create () in
    let child_action file =
      let t0 = Mtime_clock.now () in
      !ProcessPoolState.update_status (Some t0) file ;
      match capture_file ~is_binary file with
      | Ok () ->
          None
      | Error err ->
          Error.format_error file err ;
          Some ()
    in
    let child_prologue _ = Py.initialize ~interpreter:Version.python_exe () in
    let child_epilogue worker_id =
      let tenv_path = ResultsDir.get_path Temporary ^/ "child.tenv" |> DB.filename_from_string in
      let tenv_path = DB.filename_add_suffix tenv_path (ProcessPool.Worker.show_id worker_id) in
      L.debug Capture Quiet "Epilogue: writing child %a tenv to %s@\n" ProcessPool.Worker.pp_id
        worker_id (DB.filename_to_string tenv_path) ;
      Tenv.write child_tenv tenv_path ;
      Py.finalize () ;
      tenv_path
    in
    (child_action, child_prologue, child_epilogue)
  in
  L.progress "Expecting to capture %d files@\n" n_files ;
  (* TODO(vsiles) keep track of the number of success / failures like Hack *)
  let n_captured, n_error = (ref 0, ref 0) in
  let tasks () =
    ProcessPool.TaskGenerator.of_list files ~finish:(fun result _ ->
        match result with
        | Some () ->
            incr n_error ;
            None
        | None ->
            incr n_captured ;
            None )
  in
  let jobs =
    let per_worker = 100 in
    min ((per_worker + n_files) / per_worker) Config.jobs
  in
  L.debug Capture Quiet "Preparing to capture with %d workers@\n" jobs ;
  let runner = Tasks.Runner.create ~jobs ~child_prologue ~f:child_action ~child_epilogue tasks in
  let child_tenv_paths = Tasks.Runner.run runner in
  (* Merge worker tenvs into a global tenv *)
  let child_tenv_paths =
    Array.mapi child_tenv_paths ~f:(fun child_num tenv_path ->
        match tenv_path with
        | Some tenv_path ->
            tenv_path
        | None ->
            L.die ExternalError "Child %d did't return a path to its tenv" child_num )
  in
  L.progress "Success: %d files@\n" !n_captured ;
  L.progress "Failure: %d files@\n" !n_error ;
  L.progress "Merging type environments...@\n%!" ;
  MergeCapture.merge_global_tenv ~normalize:true (Array.to_list child_tenv_paths) ;
  Array.iter child_tenv_paths ~f:(fun filename -> DB.filename_to_string filename |> Unix.unlink)


let capture input =
  match input with
  | Bytecode {files} ->
      capture_files ~is_binary:true files ;
      L.progress "Finished capture.@\n"
  | Files {prog; args} ->
      if not (String.equal prog "python3") then
        L.die UserError "python3 should be explicitly used instead of %s." prog ;
      let files =
        match Config.python_files_index with
        | Some f -> (
          match Utils.read_file f with
          | Ok lines ->
              lines @ args
          | Error error ->
              L.die UserError "Error reading the python input files index '%s': %s@." f error )
        | None ->
            args
      in
      capture_files ~is_binary:false files ;
      L.progress "Finished capture.@\n"
