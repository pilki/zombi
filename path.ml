open Filename
open Printf


type path' =
    { base_dir: string;
      path: string list;
      (* memoisation *)
      path_string: string;
      complete_string: string
    }
type 'a path = path'

type implicit = [`implicit]

type absolute = [`absolute]


type src
type bld
type 'a relative = [`relative of 'a]

type 'a notimplicit = [ absolute | 'a relative]

type 'a t = 'a path

type imppath = implicit path
type abspath = absolute path
type 'a rpath = 'a relative path
type srcparam = src relative
type srcpath = src rpath
type bldparam = bld relative
type bldpath = bld rpath



let root_dir = Options.root_dir ^ dir_sep
let src_dir = root_dir
let build_dir =
  if is_relative !Options.build_dir then
    root_dir ^ !Options.build_dir ^ dir_sep
  else
    !Options.build_dir ^ dir_sep


let construct_path_string p = String.concat dir_sep p
let construct_complete_path base_dir path_string =
  base_dir ^ path_string


(* the unit is here only for the optional argument...*)
let construct_path ?path_string ?complete_string ~base_dir ~path ()=
  let path_string =
    match path_string with
    | Some ps -> ps
    | None -> construct_path_string path in
  let complete_string =
    match complete_string with
    | Some cs -> cs
    | None -> construct_complete_path base_dir path_string in
  {base_dir; path; path_string; complete_string}

(* takes a string and transform it into a 'a path. not exported *)
let split p =
  let rec go p acc =
    let dir = dirname p in
    if dir = p then dir, acc
    else go dir (basename p :: acc)
  in go p []


let imppath_of_string s : imppath =
  if not (is_implicit s) then failwith (sprintf "%s is not an implicit path" s);
  let base_dir, path = split s in
  construct_path ()
    ~base_dir:""
    ~path:path

let mki = imppath_of_string

let abspath_of_string s : abspath =
  let s = if is_relative s then root_dir ^ dir_sep ^ s else s in
  construct_path ()
    ~base_dir:s
    ~path:[]
    ~path_string:""
    ~complete_string:s
let mka = abspath_of_string

let in_build_dir (ip: imppath) : bldpath =
  construct_path ()
    ~base_dir:build_dir
    ~path:ip.path
    ~path_string:ip.path_string

let in_src_dir (ip: imppath) : srcpath =
  construct_path ()
    ~base_dir:src_dir
    ~path:ip.path
    ~path_string:ip.path_string

let implicit_of_relative (rp: 'a rpath) : imppath =
  construct_path ()
    ~base_dir:""
    ~path:rp.path
    ~path_string:rp.path_string
    ~complete_string:rp.path_string

let ior = implicit_of_relative

let string_of_path (p: 'a path) =
  p.complete_string

let to_string = string_of_path
let short_string p = p.path_string

let quoted_string_of_path (p: [< 'a notimplicit] path) =
  quote (string_of_path p)

let to_quoted = quoted_string_of_path

let short_quoted p = quote (short_string p)

(* add the extension at the end *)
let add_extension (p: 'a path) (ext: string): 'a path =
  let rec aux = function
    | [] -> ["." ^ext]
    | [base] -> [Printf.sprintf "%s.%s" base ext]
    | h :: t -> h :: aux t
  in
  construct_path ()
    ~path:(aux p.path)
    ~base_dir:p.base_dir


let exists (p: [< 'a notimplicit] path) =
  Sys.file_exists (to_string p)

let digest (p: [< 'a notimplicit] path) =
  Digest.file (to_string p)

module Operators = struct
  (* concat two paths *)
  let ( / ) (p1: 'a path) (p2: imppath) =
    construct_path ()
      ~base_dir:p1.base_dir
      ~path:(p1.path @ p2.path)
  (* add an extension to the path *)
  let ( -.- ) p ext = add_extension p ext
end
open Operators

module OrderedPathname =
  struct
    type t = path'
    let compare p1 p2 =
      String.compare (to_string p1) (to_string p2)
  end

module PSet = 
  struct 
    include Set.Make(OrderedPathname)
    type 'p u = t
  end

module PMap = 
  struct 
    include Map.Make(OrderedPathname)
    type ('p, 'a) u = 'a t
  end
