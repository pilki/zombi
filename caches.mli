
(** when a file has been built, we want to know how
    * [`Command cmd] is for most ways of building a target
    * [`Other explaination] is for the rest.
     For example a copy from the source dir, an echo in the file, etc
*)

type command_descr = [`Command of string |`Other of string]

type writing_witness

