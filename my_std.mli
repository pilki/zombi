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

open Path


val with_input_path : ?bin:bool -> [< 'ni notimplicit] path -> (in_channel -> 'a) -> 'a
val with_output_path : ?bin:bool -> bldpath -> (out_channel -> 'a) -> 'a
