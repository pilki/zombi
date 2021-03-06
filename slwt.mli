(** implementation of "super light weight threads". In normal mode,
    they are Lwt (written by Jerome Vouillon) threads. In "no unix"
    mode, they must be reimplemented more simply *)

(** type of threads *)
type 'a slwt = 'a Lwt.t
type 'a t = 'a slwt

(** wakener *)
type 'a wakeup_slwt

(** the monad *)
val return : 'a -> 'a slwt
val fail : exn -> 'a slwt
val bind : 'a slwt -> ('a -> 'b slwt) -> 'b slwt

(** usefull syntax for bind *)
val ( >>= ) : 'a slwt -> ('a -> 'b slwt) -> 'b slwt
val ( =<< ) : ('a -> 'b slwt) -> 'a slwt -> 'b slwt

val map : ('a -> 'b) -> 'a slwt -> 'b slwt
(** syntax for map *)
val ( >|= ) : 'a slwt -> ('a -> 'b) -> 'b slwt
val ( =|< ) : ('a -> 'b) -> 'a slwt -> 'b slwt

(** catch an exception : should replace try ... with *)
val catch : (unit -> 'a slwt) -> (exn -> 'a slwt) -> 'a slwt

(** like bind, except that
    * the 'a slwt is "suspended"
    * a third argument is the exception handler *)
val try_bind : (unit -> 'a slwt) -> ('a -> 'b slwt) ->
  (exn -> 'b slwt) -> 'b slwt


(*val finalize : (unit -> 'a slwt) -> (unit -> unit t) -> 'a slwt
val choose : 'a slwt list -> 'a slwt
val join : unit t list -> unit t
val ( <?> ) : 'a slwt -> 'a slwt -> 'a slwt
val ( <&> ) : unit t -> unit t -> unit t
val ignore_result : 'a slwt -> unit *)

(** a fresh pair of a sleaping thread and a wakener *)
val wait : unit -> 'a slwt * 'a wakeup_slwt

(** wake up the sleaping thread with a value *)
val wakeup : 'a wakeup_slwt -> 'a -> unit

(** wake up the sleaping thread with an exception *)
val wakeup_exn : 'a wakeup_slwt -> exn -> unit

(*
type 'a state = 'a Lwt.state = Return of 'a | Fail of exn | Sleep
val state : 'a slwt -> 'a state
exception Canceled
val task : unit -> 'a slwt * 'a wakeup_slwt
val on_cancel : 'a slwt -> (unit -> unit) -> unit
val cancel : 'a slwt -> unit
val pick : 'a slwt list -> 'a slwt
val protected : 'a slwt -> 'a slwt
val pause : unit -> unit t
val wakeup_paused : unit -> unit
val poll : 'a slwt -> 'a option
val apply : ('a -> 'b slwt) -> 'a -> 'b slwt
*)

(** helpers on lists. _s means "in sequence" an _p "in parallel" *)

val iter_s : ('a -> unit slwt) -> 'a list -> unit slwt
val iter_p : ('a -> unit slwt) -> 'a list -> unit slwt
val map_s : ('a -> 'b slwt) -> 'a list -> 'b list slwt
val map_p : ('a -> 'b slwt) -> 'a list -> 'b list slwt




(** [delay_bind] allows to produce a sleeping ['b slwt] from a bind, and
    to waken it with the returned function, Usefull if you want to
    immediatly put the thread in a cache before you let it do
    anything *)

val delay_bind : 'a slwt -> ('a -> 'b slwt) -> 'b slwt * (unit -> unit)
val ( >>>= ) : 'a slwt -> ('a -> 'b slwt) -> 'b slwt * (unit -> unit)


(** [delay_launch f] will run f when you pass its argument to the
second member of it's result *)

(* it should have the type *)
(*val delay_launch : ('a -> 'b slwt) -> ('b slwt * ('a -> unit))*)
(* but this might be way harder to implement. We don't need more than
   the following type for now on *)
val delay_launch : (unit -> 'b slwt) -> ('b slwt * (unit -> unit))


(** Pools of threads *)
module Pool :
    sig
      type 'a t
      val create :
        (** number of threads in the pool *)
        int ->
        (**  XXX I don't know what it's for *)
	?check:('a -> (bool -> unit) -> unit) ->
        (** the function creating new 'a s*)
        (unit -> 'a slwt) ->
        (** resulting pool*)
        'a t
      (** [use p f] runs the function [f] when a thread in [p] is
          available*)
      val use : 'a t -> ('a -> 'b slwt) -> 'b slwt
    end

(*
(** locks for mutual exclusion *)
module Lock :
  sig
    (** type of locks*)
    type t
    (** witness of lock*)
    type w
    val create : unit -> t

    (** [lock] waits for the lock to be available and returns a witness
       that you actually are the owner of the lock *)

    val lock : t -> w slwt
    val unlock : w -> unit

    (** [is_valid w t] checks that w is a valid witness for t, meaning
     that it is a witness of t and it has not been used to unlock t
     yet. [assert_valid] does the same, but fails with an error
     message if the witness is not valid*)

    val is_valid : w -> t -> bool
    val assert_valid : w -> t -> unit
    val is_locked : t -> bool
    val with_lock : t -> (unit -> 'a slwt) -> 'a slwt
    val with_lock_w : t -> (w -> 'a slwt) -> 'a slwt
  end
*)
module Mutex : module type of Lwt_mutex

module Slwt_process : module type of Lwt_process
