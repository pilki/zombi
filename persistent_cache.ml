let cache_version = "1"

open Printf
open My_std
open Log
open Path

(* after initialisation, old_digests contains the digests of the file
    after the last run of ocamlbuild, and will *not* been modified
    afterward. new_digests contains the same digest, but will be
    modified during the run (when a new file is built for example),
    and will then be outputed by the finalize function. This way, we
    maintain the invariant that it is safe (even if not fast) when you
    want to check if a file has been modified to compare its hash from
    what's in the old cache *)

type digest = Digest.t
type digest_cache = (string, (string, digest) Hashtbl.t) Hashtbl.t
type deps_cache = (string, imppath list) Hashtbl.t

let old_digests: digest_cache option ref = ref None
let new_digests: digest_cache option ref = ref None

let old_deps: deps_cache option ref = ref None
let new_deps: deps_cache option ref = ref None

let _zombi_caches =
  Path.in_build_dir (Path.mki "_zombi_caches")



let finalize () =
  match !new_digests, !new_deps with
  | None, _ | _, None ->
      failbug
	"We call Peristent_cache.finalize, but the cache has not been initialized" 
  | Some ndig, Some ndeps  ->
      with_output_path ~bin:true _zombi_caches begin fun oc ->
	output_string oc (cache_version ^ "\n");
	output_string oc (Sys.ocaml_version ^ "\n");
	output_string oc ((string_of_int Sys.word_size) ^ "\n");
	Marshal.to_channel oc ndig [];
	Marshal.to_channel oc ndeps []
      end

let create_caches () =
  old_digests := None;
  old_deps := None;
  new_digests := Some (Hashtbl.create 5);
  new_deps := Some (Hashtbl.create 123)

let init () =
  if (!old_digests <> None) || (!new_digests <> None) then
    failbug "The cache must be initialised only *one* time.";
  if Path.exists _zombi_caches then begin
    with_input_path _zombi_caches begin fun ic ->
      try
	let cv = input_line ic in
	let ocv = input_line ic in
	let ws = input_line ic in
	if cv <> cache_version then
	  (message "The cache version changed. Creating a new cache.";
	   create_caches ())
	else if ocv <> Sys.ocaml_version then
	  (message "The ocaml version changed. Creating a new cache.";
	   create_caches ())
	else if ws <> (string_of_int Sys.word_size) then
	  (message "The size of words is not the same. Creating a new cache.";
	   create_caches ())
	else begin
          (* getting the old cache from the file*)
	  let odig = Marshal.from_channel ic in
          (* copying it the the new cache *)
	  let ndig = Hashtbl.create 5 in
	  Hashtbl.iter (fun k t -> Hashtbl.add ndig k (Hashtbl.copy t)) odig;
	  old_digests := Some odig;
	  new_digests := Some ndig;
	  let odeps = Marshal.from_channel ic in
	  old_deps := Some odeps;
	  new_deps := Some (Hashtbl.copy odeps)
	end
      with
      | End_of_file ->
	  (message "The cache has been corrupted or the format changed. Creating a new one";
	   create_caches ())
    end
  end
  else begin
    message "No cache found. Create new one";
    create_caches ()
  end;
  at_exit finalize




type ('key, 'content) persistent_cache =
    { get_old: 'key -> 'content;
      safe_get_old: 'key -> 'content option;
      put_new: 'key -> 'content -> unit;
      delete_new: 'key -> unit
    }


let get_digest_cache name to_string =
  
  let get_old, safe_get_old =
    match !old_digests with
    | None ->
	(fun _ -> raise Not_found),
	(fun _ -> None)
    | Some od ->
	try
	  let ht = Hashtbl.find od name in
	  (fun k -> Hashtbl.find ht (to_string k)),
	  (fun k ->
	    try Some (Hashtbl.find ht (to_string k))
	    with
	    | Not_found -> None)
	with
	| Not_found -> 
	    (fun _ -> raise Not_found),
	    (fun _ -> None)
  in
  
  match !new_digests with
  | None -> failbug_pf
	"The cache %s has been asked before initialisation of cache" name
  | Some nd ->
      let ht =
	try Hashtbl.find nd name
	with
	  Not_found -> begin
	    let ht = Hashtbl.create 123 in
	    Hashtbl.add nd name ht;
	    ht
	  end in
      let put_new k d =
	Hashtbl.replace ht (to_string k) d in
      let delete_new k =
	Hashtbl.remove ht (to_string k) in
      {get_old; safe_get_old; put_new; delete_new}


let get_deps_cache () =  
  let get_old, safe_get_old =
    match !old_deps with
    | None ->
	(fun _ -> raise Not_found),
	(fun _ -> None)
    | Some odeps ->
	(fun ip -> List.map in_build_dir (Hashtbl.find odeps (short_string ip))),
	(fun ip ->
	  try Some (List.map in_build_dir (Hashtbl.find odeps (short_string ip)))
	  with
	  | Not_found -> 
	    None)
  in
  match !new_deps with
  | None -> failbug
	"The dependencies cache has been asked before initialisation of cache"
  | Some ndeps ->
      let put_new ip deps =
	Hashtbl.replace ndeps (short_string ip) (List.map ior deps) in
      let delete_new ip =
	Hashtbl.remove ndeps (short_string ip) in
      {get_old; safe_get_old; put_new; delete_new}



