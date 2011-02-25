open Log

type 'a slwt = 'a Lwt.t
type 'a wakeup_slwt = 'a Lwt.u

include Lwt
include Lwt_list

module Pool = Lwt_pool



let delay_bind t f =
  let t1, u1 = wait () in
  ((t1 >>= fun () ->
    t >>= f),
   wakeup u1)

let (>>>=) = delay_bind

let delay_launch a =
  let t1, u1 = wait () in
  (t1 >>= a, wakeup u1)

(*
module Lock =
struct

  let new_tag =
    let r = ref 0 in
    fun () -> incr r; !r


  type t =
      { mutex: Lwt_mutex.t;
        tag: int}

  type w =
      { t: t;
        mutable valid: bool}

  let create () =
    { mutex = Lwt_mutex.create ();
      tag = new_tag ()}

  let lock t =
    Lwt_mutex.lock t.mutex >>= fun () ->
    return
        { t;
          valid = true}

  let unlock w =
    w.valid <- false;
    Lwt_mutex.unlock w.t.mutex

  let is_valid w t =
    w.t.tag = t.tag && w.valid

  let assert_valid w t =
    if not (w.t.tag == t.tag) then
      error "A lock has been used with the wrong witness"
    else
      if not w.valid then
        failwith "An invalid lock witness has been used"

  let is_locked t = Lwt_mutex.is_locked t.mutex

  let with_lock t f =
    lock t >>= fun w ->
      catch

        (fun () ->
          f () >>= fun x ->
          unlock w;
          return x)

        (fun e -> unlock w; fail e)

  let with_lock_w t f =
    lock t >>= fun w ->
      catch

        (fun () ->
          f w >>= fun x ->
          unlock w;
          return x)

        (fun e -> unlock w; fail e)

end
*)

module Mutex = Lwt_mutex
module Slwt_process = Lwt_process
