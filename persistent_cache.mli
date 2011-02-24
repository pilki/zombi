(** init should be called once, and only once! For now, I don't know
    who calls it. Should be written here when someone does it *)

val init: unit -> unit


type ('key, 'content) persistent_cache =
    { get_old: 'key -> 'content;
      safe_get_old: 'key -> 'content option;
      put_new: 'key -> 'content -> unit;
      delete_new: 'key -> unit
    }

val get_digest_cache:
  string -> ('key -> string) -> ('key, Digest.t) persistent_cache

val get_deps_cache: unit -> (Path.bldpath, Path.bldpath list) persistent_cache
