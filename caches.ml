open Pathname
open Persistent_cache
open Printf
open Log

(* internal files *)


type command_descr = [`Command of string |`Other of string]
type cache_reason = [command_descr | `NotBuiltYet | `Removed]

let string_of_command_descr = function
  | `Command s ->
      sprintf "Command: %s" s
  | `Other s -> s


(* perm_cache_build is the persistent cache of the digests of the
   files inside the build dir as they were after the last build *)

let perm_cache_build = new_digest_cache "build dir"

(* current_digest_cache is the cache of the digests of the files in
   the build dir as they are now *)

let current_digest_build : (string, (string * cache_reason)) Hashtbl.t = Hashtbl.create 103

(* for the cache, we only accept bldpathname, but we use the implicit
   part of the path. It allows the source dir/build dir to be moved
   and/or access via a different path during the next run *)

let implicit_name (p: bldpathname) = to_string (ior p)

let get_current_bld (p: bldpathname) =
  try Some (Hashtbl.find current_digest_build (implicit_name p))
  with
    Not_found -> None

let set_current_bld (p: bldpathname) digest reason =
  let iname = implicit_name p in
  Hashtbl.replace current_digest_build iname (digest, reason);
  perm_cache_build.put_new iname digest

let delete_current_bld (p: bldpathname) =
  let iname = implicit_name p in
  Hashtbl.replace current_digest_build iname (digest, reason);
  perm_cache_build.put_new iname digest



let touch (p: bldpathname) (c: command_descr) =
  let fn = to_string p in
  if not (exists p) then
    failwith (sprintf "The file %s can't be touched. It does not exist" fn);
  let new_digest = Digest.file fn in
  (* see if a digest already exists *)
  begin match get_current_bld p with
  | None -> ()
  | Some (old_digest, reason) ->
      begin match reason with

      (* the digest has probably been hasked, but the file has not
	  been touched yet*)
      | `NotBuiltYet -> ()

      (* the file has already been touched *)
      | #command_descr as reason ->
	  let err_msg =
	    sprintf "The file %s has been built twice, once with\n  %s\nand the second time with\n  %s\n"
	      (implicit_name p) (string_of_command_descr reason) (string_of_command_descr c) in
	  if old_digest = new_digest then
	    warning (err_msg ^ "But the produced files are the same")
	  else
	    error (err_msg ^ "And the produced files are disjoint")
      end
  end;
  (* we update the digest cache *)
  set_current_bld p new_digest (c:> cache_reason)



let perm_cache_external = Persistent_cache.new_digest_cache "build dir"
