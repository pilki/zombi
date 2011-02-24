type 'a slwt = 'a Lwt.t
type 'a t = 'a slwt
type 'a wakeup_slwt
val return : 'a -> 'a slwt
val fail : exn -> 'a slwt
val bind : 'a slwt -> ('a -> 'b slwt) -> 'b slwt
val ( >>= ) : 'a slwt -> ('a -> 'b slwt) -> 'b slwt
val ( =<< ) : ('a -> 'b slwt) -> 'a slwt -> 'b slwt
val map : ('a -> 'b) -> 'a slwt -> 'b slwt
val ( >|= ) : 'a slwt -> ('a -> 'b) -> 'b slwt
val ( =|< ) : ('a -> 'b) -> 'a slwt -> 'b slwt
val catch : (unit -> 'a slwt) -> (exn -> 'a slwt) -> 'a slwt
val try_bind : (unit -> 'a slwt) -> ('a -> 'b slwt) -> (exn -> 'b slwt) -> 'b slwt
(*val finalize : (unit -> 'a slwt) -> (unit -> unit t) -> 'a slwt
val choose : 'a slwt list -> 'a slwt
val join : unit t list -> unit t
val ( <?> ) : 'a slwt -> 'a slwt -> 'a slwt
val ( <&> ) : unit t -> unit t -> unit t
val ignore_result : 'a slwt -> unit *)
val wait : unit -> 'a slwt * 'a wakeup_slwt
val wakeup : 'a wakeup_slwt -> 'a -> unit
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


(** [delay_launch f] will run f when you pass its argument to the second member of it's result *)
(*val delay_launch : ('a -> 'b slwt) -> ('b slwt * ('a -> unit))*)
val delay_launch : (unit -> 'b slwt) -> ('b slwt * (unit -> unit))


module Pool :
    sig
      type 'a t
      val create : int ->
	?check:('a -> (bool -> unit) -> unit) -> (unit -> 'a slwt) -> 'a t
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
