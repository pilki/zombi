(** this module takes care of most things regarding files in the build dir :
    * caching of digest, during a run of zombi or between runs
    * locks to have the right to write or read a file
    *)

open Path
open Slwt


type command_descr = [`Command of string |`Other of string | `Deleted | `FromSrc]




type bld_write_lock

(* before writing a file, it is mandatory to acquire a lock on that
   file. The returned boolean expresses that the lock is already
   available *)

val acquire_lock: bldpath -> bool * (bld_write_lock slwt)

(* to avoid dead locks, when multiple locks must be acquired, they
   are all acquired at the same time, or it yields and try again
   later. As for [acquire_lock], the returned boolean expresses if
   the locks are directly available *)

val acquire_locks: bldparam PSet.u ->
  bool * ((bldparam, bld_write_lock) PMap.u slwt)


(* when a lock is not needed anymore, it must be released. The version
   for many lock is a little bit better than itering on the single
   version*)

val release_lock: bld_write_lock -> unit
val release_locks: (bldparam, bld_write_lock) PMap.u -> unit


(* what file is locked ?*)
val get_path: bld_write_lock -> bldpath

(* a lock is valid until it is released *)
val is_valid: bld_write_lock -> bool

(* same as [is_valid] but produces an error and stop the program if not *)
val assert_valid: bld_write_lock -> unit

(* same as [assert_valid] but also checks that the lock concerns the path-name *)
val assert_valid_pn: bld_write_lock -> bldpath -> unit


(* when a file has been built, we want to know how (via command_descr)
    * [`Command cmd] is for most ways of building a target
    * [`Deleted] when the file has been deletede
    * [`Other explaination] is for the rest.
     For example a copy from the source dir, an echo in the file, etc
*)

val touch: bld_write_lock -> command_descr -> unit



(* The cache part *)

(* [is_modified f] tells if the file f has been modified since the last run of zombi *)
val is_modified: bldpath -> bool

(* the digest of the file *)
val digest: bldpath -> Digest.t
