open Path
open Slwt
open Persistent_cache
open Printf
open Log

(* XXXXXX to be moved to options somewhere *)
let use_fast_check = ref true
let use_hard_link = ref true



type command_descr = [`Command of string |`Other of string | `Deleted | `FromSrc]
type cache_reason = [command_descr | `NotBuiltYet]


(* persistent cache of digest of files.*)
let pers_cache_build  = get_digest_cache
    "build dir digest"
    (fun (p: bldpath) -> short_string p)

(* persistent cache of digest of stats *)
(* if a file is in this cache, it must have been moved from the build
   dir. So if a file is "touched" otherwied, it must be removed from here
*)

let pers_cache_stats = get_digest_cache
    "src dir stats"
    (fun (p: srcpath) -> short_string p)

(* the pool of locks. Could be a hashtbl or a ref on a path map *)
let locks_pool = Hashtbl.create 123


(* val wait_release : unit -> unit slwt
wait for any lock to be released

val do_release: unit -> unit
to be called after releasing locks *)

let wait_release, do_release =
  let r = ref (wait ()) in
  ((fun () -> fst !r),
   (fun () ->
     let _, u = !r in
     r := wait ();
     wakeup u ()))


(* if the lock already exists, it is returned. If not, a new one is created *)
let get_lock (pn: bldpath) =
  try Hashtbl.find locks_pool pn
  with
  | Not_found ->
      (* it's the first time a lock for this pn has been asked. We create it *)
      let new_lock = Mutex.create () in
      Hashtbl.add locks_pool pn new_lock;
      (* DEBUG: check of integrity*)
      let if_you_modified =
	"If you modified files in the build dir (which is not a good idea), this"
	^ "message is normal. If you deed not, it might be a bug." in
      begin match pers_cache_build.safe_get_old pn with
      | None -> if exists pn then
          warning_pf
	    "The file %s is in the build dir but not in the cache of files in the build dir. %s"
            (short_string pn) if_you_modified
      | Some old_digest ->
          if exists pn then
            begin
              let new_digest = digest pn in
              if new_digest <> old_digest then
                warning_pf
		  "The file %s in the build dir has a different hash in the cache. %s"
		  (short_string pn) if_you_modified
            end
          else
            warning_pf
	      "The file %s is in the cache but not in the build dir. %s"
	      (short_string pn) if_you_modified
      end;
      new_lock



type bld_write_lock =
    {lock_path: bldpath;
     mutable valid: bool;
     mutex: Mutex.t}


let acquire_lock lock_path =
  let l = get_lock lock_path in
  (not (Mutex.is_locked l),
   Mutex.lock l >>= fun () -> return
     { lock_path;
       valid = true;
       mutex = l})


let acquire_locks set =
  let paths = PSet.elements set in
  let locks = List.map (fun lock_path ->
    {lock_path;
     valid = true;
     mutex = get_lock lock_path}) paths in
  let map =
    List.fold_left
      (fun acc l -> PMap.add l.lock_path l acc) PMap.empty locks in
  let rec aux () =
    if List.exists (fun bwl -> Mutex.is_locked bwl.mutex) locks then
      (* if one of the lock is not available, we wait *)
      wait_release () >>= fun ()->
	aux ()
    else
      (* if all locks are available, we lock them all*)
      (iter_s (fun bwl -> Mutex.lock bwl.mutex) locks >>= fun () ->
       return map)
  in
  (not (List.exists (fun bwl -> Mutex.is_locked bwl.mutex) locks),
   aux ())

let release_lock bwl =
  if not bwl.valid then
    failbug_pf "A lock for path %s has been released twice"
      (short_string bwl.lock_path);
  bwl.valid <- false;
  Mutex.unlock bwl.mutex;
  do_release ()

let release_locks map =
  let not_valid = (PMap.filter (fun _ bwl -> not bwl.valid) map) in
  if not (PMap.is_empty not_valid) then
    failbug_pf "A lock for path %s has been released twice"
      (short_string (snd (PMap.choose not_valid)).lock_path);
  PMap.iter (fun _ bwl ->
    bwl.valid <- false;
    Mutex.unlock bwl.mutex) map;
  do_release ()

let get_path bwl = bwl.lock_path


let is_valid bwl = bwl.valid
let assert_valid bwl =
  if not bwl.valid then
    failbug_pf "A lock for for path %s was supposed to be valid but is not"
      (short_string bwl.lock_path)

let assert_valid_pn bwl p =
  if bwl.lock_path <> p then
    failbug_pf "A lock for path %s was supposed to be for path %s."
      (short_string bwl.lock_path) (short_string p);
  assert_valid bwl


