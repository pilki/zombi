


type log_level = [`ZBug | `Error | `Warning | `Message | `Command]
val log_levels: log_level list



(** decide what level will be printed on the standard output. Takes the [n] firsts in log_level list. It means:
    0: nothing
    1: only errors
    2: errors and warnings
    etc.
*)

val show_verbose: int -> unit


(** Allow to chose more specifically what will be shown *)
val show_all: unit -> unit
val show_zbugs: unit -> unit
val show_errors: unit -> unit
val show_warnings: unit -> unit
val show_messages: unit -> unit
val show_commands: unit -> unit

(** [log l m] log the message m at level m *)
val log: log_level -> string -> unit
val bug: string -> unit
val error: string -> unit
val warning: string -> unit
val message: string -> unit
val command: string -> unit

(** printf style version of the former *)
val bug_pf: ('a, unit, string, unit) format4 -> 'a
val error_pf: ('a, unit, string, unit) format4 -> 'a
val warning_pf: ('a, unit, string, unit) format4 -> 'a
val message_pf: ('a, unit, string, unit) format4 -> 'a
val command_pf: ('a, unit, string, unit) format4 -> 'a

(** a replacement for failwith that really fails. Avoid all the
   catching nonsens. [fail] just... fails*)

val fail: unit -> 'a
val failwith: string -> 'a
val failwith_pf: ('a, unit, string, 'b) format4 -> 'a
val failbug: string -> 'a
val failbug_pf: ('a, unit, string, 'b) format4 -> 'a

(** this function should be called by option, and no one else*)
val set_log_file: string -> unit


