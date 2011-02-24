open Slwt
open Slwt_process

let max_jobs = ref 1

type process_status = (*Unix.process_status =*)
| WEXITED of int (* The process terminated normally by exit; the argument is the return code. *)
| WSIGNALED of int (* The process was killed by a signal; the argument is the signal number. *)
| WSTOPPED of int (* The process was stopped by a signal; the argument is the signal number. *)

let process_status_from_unix = function
  | Unix.WEXITED i -> WEXITED i
  | Unix.WSIGNALED i -> WSIGNALED i
  | Unix.WSTOPPED i -> WSTOPPED i


(* reference on the potential pool of [()] used to limit the number of concurent jobs *)
let run_para_ref = ref None


(* [run_para] takes a suspension producing a ['a slwt] and runs it
   potentially later if too many jobs are runing*)

let rec run_para (act: unit -> 'a slwt) : 'a slwt=
  match !run_para_ref with
  | None -> (* the pool has not been set yet*)
      run_para_ref := Some
	(if !max_jobs <= 0 then (*infinit number of jobs *)
	  None
	else (* !jobs at most *)
	  Some (Pool.create !max_jobs (fun () -> return ())));
      (* now that the pool has been set, let's try again ! *)
      run_para act
  | Some None ->
    (* infinite number of jobs => action lounched now *)
      act ()
  | Some (Some pool) ->
    (* finite number of jobs => wait for a slot to be available *)
      Pool.use pool act


let exec_scmd_force scmd =
  let proc = open_process_full (shell scmd) in
  let stdoutput = Lwt_io.read proc#stdout in
  let erroutput = Lwt_io.read proc#stderr in
  stdoutput >>= fun stdoutput ->
(*  lwt stdoutput = stdoutput in*)
  erroutput >>= fun erroutput ->
  proc#close >>= fun status ->
  return (Some stdoutput, Some erroutput, process_status_from_unix status)


let exec_scmd scmd =
  run_para (fun () -> exec_scmd_force scmd)

(* helper to show the failure of a command *)

let display s = Log.display (fun oc -> output_string oc s)
let odisplay = function
  | None -> ()
  | Some s -> display s

(* if a command fail, we fail immediatly *)
let fail_scmd scmd out err = function
  | WEXITED n ->
      display scmd; odisplay err; odisplay out;
      Log.display (fun oc -> Printf.fprintf oc "Command exited with code %i\n" n);
      exit n
  | WSIGNALED signal ->
      display scmd; odisplay err; odisplay out;
      Log.display (fun oc -> Printf.fprintf oc "Command received signal %i\n" signal);
      exit 1
  | WSTOPPED signal ->
      display scmd; odisplay err; odisplay out;
      Log.display (fun oc -> Printf.fprintf oc "Command stoped by signal %i\n" signal);
      exit 1

let simpl_exec_scmd scmd =
  exec_scmd scmd >>= fun (out, err, status) ->
  if status <> WEXITED 0 then fail_scmd scmd out err status else return ()
