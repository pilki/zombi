(***********************************************************************)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Nicolas Pouillard *)

open Path
let with_input_file ?(bin=false) x f =
  let ic = (if bin then open_in_bin else open_in) x in
  try let res = f ic in close_in ic; res with e -> (close_in ic; raise e)

let with_output_file ?(bin=false) x f =
  let oc = (if bin then open_out_bin else open_out) x in
  try let res = f oc in close_out oc; res with e -> (close_out oc; raise e)


let with_input_path ?(bin=false) (p: [< 'a notimplicit] path) f =
  with_input_file ~bin:bin (Path.to_string p) f

let with_output_path ?(bin=false) (p:bldpath) f =
  with_output_file ~bin:bin (Path.to_string p) f

