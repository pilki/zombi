open Slwt
(** maximum number of parallel job. Should be set before the first
    command is run, otherwhise will stay at value 1 forever *)

val max_jobs : int ref

type process_status = (*Unix.process_status =*)
| WEXITED of int (* The process terminated normally by exit; the argument is the return code. *)
| WSIGNALED of int (* The process was killed by a signal; the argument is the signal number. *)
| WSTOPPED of int (* The process was stopped by a signal; the argument is the signal number. *)



(** [run_para] takes a suspension producing a ['a slwt] and runs it
   potentially later if too many jobs are runing*)

val run_para : (unit -> 'a slwt) -> 'a slwt


(** execute a simpl command and return its output, error and
   process_status when finished. Potentially wait for other jobs to
   terminate before runing the command *)
val exec_scmd :
  string -> (string option * string option * process_status) slwt

(** same as exec_scmd, but calls fail_scmd if the command did not
    return with code 0, and discard the outputs *)

val simpl_exec_scmd: string -> unit slwt


(** like exec_simpl_cmd, but is executed immediatly *)
val exec_scmd_force :
  string -> (string option * string option * process_status) slwt


(** helper to show the failure of a command and stop the
    program. Takes the name of the command, the output, the error and
    the process status*)

val fail_scmd : string -> string option -> string option -> process_status -> 'a
