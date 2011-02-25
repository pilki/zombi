(* Original author: Alexandre Pilkiewicz *)
open Printf

(* XXX should be in a lib *)
let rec take n l =
  match l with
  | [] -> []
  | h :: t -> if n = 0 then [] else h :: (take (pred n) t)



(* those two lists *must* be the same *)

type log_level = [`ZBug | `Error | `Warning | `Message | `Command]
let log_levels = [`ZBug ; `Error ; `Warning ; `Message ; `Command]
let log_names = ["Bug of Zombi"; "Error"; "Warning"; "Message"; "Command"]

let name_of_level =
  let assoc_list = List.combine log_levels log_names in
  fun level ->
    List.assoc level assoc_list

let (shown_levels: log_level list ref) = ref []

let show_verbose n =
  shown_levels := [];
  List.iter (fun l -> shown_levels := l :: !shown_levels) (take n log_levels)

let show_all () = show_verbose (List.length log_levels)

let show_zbugs () = shown_levels := `ZBug :: !shown_levels
let show_errors () = shown_levels := `Error :: !shown_levels
let show_warnings () = shown_levels := `Warning :: !shown_levels
let show_messages () = shown_levels := `Message :: !shown_levels
let show_commands () = shown_levels := `Command :: !shown_levels


(* Handler of the log file. Set by set_log_file *)
let log_file = ref None

let log level message =
  let message = sprintf "%s: %s\n" (name_of_level level) message in
  if List.mem level !shown_levels then
    (* XXX to be modified if we change the ouptut method *)
    print_string message;
  match !log_file with
  | None -> ()
  | Some oc -> output_string oc message

let bug = log `ZBug
let error = log `Error
let warning = log `Warning
let message = log `Message
let command = log `Command

(* sprintf style versions *)
let bug_pf s =
  ksprintf bug s
let error_pf s =
  ksprintf error s
let warning_pf s =
  ksprintf warning s
let message_pf s =
  ksprintf message s
let command_pf s =
  ksprintf command s


(* failures *)
let fail () = exit 1

let failwith s =
  error s;
  fail ()

let failwith_pf s =
  ksprintf failwith s

let failbug s =
  bug s;
  fail ()

let failbug_pf s = ksprintf failbug s


let set_log_file s =
  if !log_file <> None then
    log `Warning "The log file is set twice";
  let oc = open_out s in
  log_file := Some oc;
  at_exit (fun () -> close_out oc)

