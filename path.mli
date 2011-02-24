(** abstract type of path *)
type 'a path

(** we have 4 different kinds of path:
    - implicit path name, like blah.ml or foo/baz.o
    - absolute path names, like /foo/bar
    - relative path names, of too kinds
      * in the build dir
      * in the source dir *)
(** fantom types (abstract) *)
type implicit = [`implicit]

type absolute = [`absolute]

type 'a relative = [`relative of 'a]
type src
type bld


type 'a notimplicit = [ absolute | 'a relative]


(** aliases *)
type 'a t = 'a path
type imppath = implicit path
type abspath = absolute path
type 'a rpath = 'a relative path
type srcparam = src relative
type srcpath = src rpath
type bldparam = bld relative
type bldpath = bld rpath


(** [imppath_of_string] expects the path to be implicit. [mki] is an alias *)
val imppath_of_string: string -> imppath
val mki: string -> imppath

(** if the path is absolute, it is not changed. If it is relative, the
    root directory is added. [mka] is an alias *)
val abspath_of_string: string -> abspath
val mka: string -> abspath

(** move implicit path to the build dir or the source dir *)
val in_build_dir: imppath -> bldpath
val in_src_dir: imppath -> srcpath


val implicit_of_relative: 'a rpath -> imppath
val ior:  'a rpath -> imppath

(** a flatten version of the path *)
val string_of_path: 'a path -> string
val to_string: 'a path -> string


(** name of a relative path, without the prefix. [short_string p]
    is the same as [to_string (ior p)]*)
val short_string: 'a rpath -> string

(** [quoted_string_of_path] returns a quoted version (see
    Filename.quote) that is not suitable to go back to paths, but
    should be used in commands. Since the goal is to use it in
    commands, and that commands should only deals with "real" files,
    it should not be implicit*)
val quoted_string_of_path: [< 'a notimplicit ] path -> string
val to_quoted: [< 'a notimplicit ] path -> string

(** [short_quoted] does not include de root directory. Usefull for
    compilation command, when you want the error message to point in
    the src dir, but the "real" file is in the build dir*)
val short_quoted: 'a rpath -> string

(** [exists] checks if the path points to a file that does exists *)
val exists: [< 'a notimplicit ] path -> bool
val digest: [< 'a notimplicit ] path -> Digest.t

module Operators: sig
  (* concat two paths *)
  val ( / ): 'a path -> imppath -> 'a path
  (* add an extension to the path *)
  val ( -.- ): 'a path -> string -> 'a path
end

module PSet :
  sig
    type 'p u
    (** The type of sets. *)

    val empty: 'p u
    (** The empty set. *)

    val is_empty: 'p u -> bool
    (** Test whether a set is empty or not. *)

    val mem: 'p path -> 'p u -> bool
    (** [mem x s] tests whether [x] belongs to the set [s]. *)

    val add: 'p path -> 'p u -> 'p u
    (** [add x s] returns a set containing all elements of [s],
       plus [x]. If [x] was already in [s], [s] is returned unchanged. *)

    val singleton: 'p path -> 'p u
    (** [singleton x] returns the one-element set containing only [x]. *)

    val remove: 'p path -> 'p u -> 'p u
    (** [remove x s] returns a set containing all elements of [s],
       except [x]. If [x] was not in [s], [s] is returned unchanged. *)

    val union: 'p u -> 'p u -> 'p u
    (** Set union. *)

    val inter: 'p u -> 'p u -> 'p u
    (** Set intersection. *)

    (** Set difference. *)
    val diff: 'p u -> 'p u -> 'p u

    val compare: 'p u -> 'p u -> int
    (** Total ordering between sets. Can be used as the ordering function
       for doing sets of sets. *)

    val equal: 'p u -> 'p u -> bool
    (** [equal s1 s2] tests whether the sets [s1] and [s2] are
       equal, that is, contain equal elements. *)

    val subset: 'p u -> 'p u -> bool
    (** [subset s1 s2] tests whether the set [s1] is a subset of
       the set [s2]. *)

    val iter: ('p path -> unit) -> 'p u -> unit
    (** [iter f s] applies [f] in turn to all elements of [s].
       The elements of [s] are presented to [f] in increasing order
       with respect to the ordering over the type of the elements. *)

    val fold: ('p path -> 'a -> 'a) -> 'p u -> 'a -> 'a
    (** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
       where [x1 ... xN] are the elements of [s], in increasing order. *)

    val for_all: ('p path -> bool) -> 'p u -> bool
    (** [for_all p s] checks if all elements of the set
       satisfy the predicate [p]. *)

    val exists: ('p path -> bool) -> 'p u -> bool
    (** [exists p s] checks if at least one element of
       the set satisfies the predicate [p]. *)

    val filter: ('p path -> bool) -> 'p u -> 'p u
    (** [filter p s] returns the set of all elements in [s]
       that satisfy predicate [p]. *)

    val partition: ('p path -> bool) -> 'p u -> 'p u * 'p u
    (** [partition p s] returns a pair of sets [(s1, s2)], where
       [s1] is the set of all the elements of [s] that satisfy the
       predicate [p], and [s2] is the set of all the elements of
       [s] that do not satisfy [p]. *)

    val cardinal: 'p u -> int
    (** Return the number of elements of a set. *)

    val elements: 'p u -> 'p path list
    (** Return the list of all elements of the given set.
       The returned list is sorted in increasing order with respect
       to the ordering [Ord.compare], where [Ord] is the argument
       given to {!Set.Make}. *)

    val min_elt: 'p u -> 'p path
    (** Return the smallest element of the given set
       (with respect to the [Ord.compare] ordering), or raise
       [Not_found] if the set is empty. *)

    val max_elt: 'p u -> 'p path
    (** Same as {!Set.S.min_elt}, but returns the largest element of the
       given set. *)

    val choose: 'p u -> 'p path
    (** Return one element of the given set, or raise [Not_found] if
       the set is empty. Which element is chosen is unspecified,
       but equal elements will be chosen for equal sets. *)

    val split: 'p path -> 'p u -> 'p u * bool * 'p u
    (** [split x s] returns a triple [(l, present, r)], where
          [l] is the set of elements of [s] that are
          strictly less than [x];
          [r] is the set of elements of [s] that are
          strictly greater than [x];
          [present] is [false] if [s] contains no element equal to [x],
          or [true] if [s] contains an element equal to [x]. *)
  end

module PMap :
  sig
    type ('p, +'a) u
    (** The type of maps from type ['p path] to type ['a]. *)

    val empty: ('p, 'a) u
    (** The empty map. *)

    val is_empty: ('p, 'a) u -> bool
    (** Test whether a map is empty or not. *)

    val mem: 'p path -> ('p, 'a) u -> bool
    (** [mem x m] returns [true] if [m] contains a binding for [x],
       and [false] otherwise. *)

    val add: 'p path -> 'a -> ('p, 'a) u -> ('p, 'a) u
    (** [add x y m] returns a map containing the same bindings as
       [m], plus a binding of [x] to [y]. If [x] was already bound
       in [m], its previous binding disappears. *)

    val singleton: 'p path -> 'a -> ('p, 'a) u
    (** [singleton x y] returns the one-element map that contains a binding [y]
        for [x].
        @since 3.12.0
     *)

    val remove: 'p path -> ('p, 'a) u -> ('p, 'a) u
    (** [remove x m] returns a map containing the same bindings as
       [m], except for [x] which is unbound in the returned map. *)

    val merge:
         ('p path -> 'a option -> 'b option -> 'c option) -> ('p, 'a) u -> ('p, 'b) u -> ('p, 'c) u
    (** [merge f m1 m2] computes a map whose keys is a subset of keys of [m1]
        and of [m2]. The presence of each such binding, and the corresponding
        value, is determined with the function [f].
        @since 3.12.0
     *)

    val compare: ('a -> 'a -> int) -> ('p, 'a) u -> ('p, 'a) u -> int
    (** Total ordering between maps.  The first argument is a total ordering
        used to compare data associated with equal keys in the two maps. *)

    val equal: ('a -> 'a -> bool) -> ('p, 'a) u -> ('p, 'a) u -> bool
    (** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are
       equal, that is, contain equal keys and associate them with
       equal data.  [cmp] is the equality predicate used to compare
       the data associated with the keys. *)

    val iter: ('p path -> 'a -> unit) -> ('p, 'a) u -> unit
    (** [iter f m] applies [f] to all bindings in map [m].
       [f] receives the key as first argument, and the associated value
       as second argument.  The bindings are passed to [f] in increasing
       order with respect to the ordering over the type of the keys. *)

    val fold: ('p path -> 'a -> 'b -> 'b) -> ('p, 'a) u -> 'b -> 'b
    (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
       where [k1 ... kN] are the keys of all bindings in [m]
       (in increasing order), and [d1 ... dN] are the associated data. *)

    val for_all: ('p path -> 'a -> bool) -> ('p, 'a) u -> bool
    (** [for_all p m] checks if all the bindings of the map
        satisfy the predicate [p].
        @since 3.12.0
     *)

    val exists: ('p path -> 'a -> bool) -> ('p, 'a) u -> bool
    (** [exists p m] checks if at least one binding of the map
        satisfy the predicate [p].
        @since 3.12.0
     *)

    val filter: ('p path -> 'a -> bool) -> ('p, 'a) u -> ('p, 'a) u
    (** [filter p m] returns the map with all the bindings in [m]
        that satisfy predicate [p].
        @since 3.12.0
     *)

    val partition: ('p path -> 'a -> bool) -> ('p, 'a) u -> ('p, 'a) u * ('p, 'a) u
    (** [partition p m] returns a pair of maps [(m1, m2)], where
        [m1] contains all the bindings of [s] that satisfy the
        predicate [p], and [m2] is the map with all the bindings of
        [s] that do not satisfy [p].
        @since 3.12.0
     *)

    val cardinal: ('p, 'a) u -> int
    (** Return the number of bindings of a map.
        @since 3.12.0
     *)

    val bindings: ('p, 'a) u -> ('p path * 'a) list
    (** Return the list of all bindings of the given map.
       The returned list is sorted in increasing order with respect
       to the ordering [Ord.compare], where [Ord] is the argument
       given to {!Map.Make}.
        @since 3.12.0
     *)

    val min_binding: ('p, 'a) u -> ('p path * 'a)
    (** Return the smallest binding of the given map
       (with respect to the [Ord.compare] ordering), or raise
       [Not_found] if the map is empty.
        @since 3.12.0
     *)

    val max_binding: ('p, 'a) u -> ('p path * 'a)
    (** Same as {!Map.S.min_binding}, but returns the largest binding
        of the given map.
        @since 3.12.0
     *)

    val choose: ('p, 'a) u -> ('p path * 'a)
    (** Return one binding of the given map, or raise [Not_found] if
       the map is empty. Which binding is chosen is unspecified,
       but equal bindings will be chosen for equal maps.
        @since 3.12.0
     *)

    val split: 'p path -> ('p, 'a) u -> ('p, 'a) u * 'a option * ('p, 'a) u
    (** [split x m] returns a triple [(l, data, r)], where
          [l] is the map with all the bindings of [m] whose key
        is strictly less than [x];
          [r] is the map with all the bindings of [m] whose key
        is strictly greater than [x];
          [data] is [None] if [m] contains no binding for [x],
          or [Some v] if [m] binds [v] to [x].
        @since 3.12.0
     *)

    val find: 'p path -> ('p, 'a) u -> 'a
    (** [find x m] returns the current binding of [x] in [m],
       or raises [Not_found] if no such binding exists. *)

    val map: ('a -> 'b) -> ('p, 'a) u -> ('p, 'b) u
    (** [map f m] returns a map with same domain as [m], where the
       associated value [a] of all bindings of [m] has been
       replaced by the result of the application of [f] to [a].
       The bindings are passed to [f] in increasing order
       with respect to the ordering over the type of the keys. *)

    val mapi: ('p path -> 'a -> 'b) -> ('p, 'a) u -> ('p, 'b) u
    (** Same as {!Map.S.map}, but the function receives as arguments both the
       key and the associated value for each binding of the map. *)


  end

