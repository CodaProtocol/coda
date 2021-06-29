open Core

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      { trust: float
      ; trust_last_updated: Core.Time.Stable.V1.t
      ; banned_until_opt: Core.Time.Stable.V1.t Core_kernel.Option.Stable.V1.t
      }

    let to_latest = Fn.id
  end
end]

module type S = sig
  val init : unit -> t

  val ban : t -> t

  val add_trust : t -> float -> t

  val to_peer_status : t -> Peer_status.t
end

(* Trust is conceptually multiplied by this factor every second. This value is
   such that trust halves in 24 hours. =~ 0.999992 *)
let decay_rate = 0.5 ** (1. /. (60. *. 60. *. 24.))

(** Module handling the data associated with a peer's trust.
    @param Now get the current time. Functored for mocking.
*)
module Make (Now : sig
  val now : unit -> Time.t
end) : S = struct
  (** Create a new blank trust record. *)
  let init () =
    {trust= 0.; trust_last_updated= Now.now (); banned_until_opt= None}

  let clamp_trust trust = Float.clamp_exn trust ~min:(-1.0) ~max:1.0

  (* Update a trust record. This must be called by every function that reads
     records, and is not exposed outside this module. *)
  let update {trust; trust_last_updated; banned_until_opt} =
    let now = Now.now () in
    let elap = Time.diff now trust_last_updated in
    let elapsed_time =
      if Time.Span.(elap >= zero) then elap else Time.Span.zero
    in
    (* ntpd or a user may have reset the system time, yielding a negative elapsed time.  in that case, clamp the elapsed time to zero*)
    let new_trust = (decay_rate ** Time.Span.to_sec elapsed_time) *. trust in
    { trust= new_trust
    ; trust_last_updated= now
    ; banned_until_opt=
        ( match banned_until_opt with
        | Some banned_until ->
            if Time.is_later banned_until ~than:(Now.now ()) then
              Some banned_until
            else None
        | None ->
            None ) }

  (** Set the record to banned, updating trust. *)
  let ban t =
    let new_record = update t in
    { new_record with
      trust= -1.0
    ; banned_until_opt= Some (Time.add (Now.now ()) Time.Span.day) }

  (** Add some trust, subtract by passing a negative number. *)
  let add_trust t increment =
    let new_record = update t in
    let new_trust = clamp_trust @@ (new_record.trust +. increment) in
    { new_record with
      trust= new_trust
    ; banned_until_opt=
        ( if Float.(new_trust <= -1.) then
          Some (Time.add new_record.trust_last_updated Time.Span.day)
        else new_record.banned_until_opt ) }

  (** Convert the internal type to the externally visible one. *)
  let to_peer_status t =
    let new_record = update t in
    match new_record.banned_until_opt with
    | None ->
        Peer_status.{trust= new_record.trust; banned= Banned_status.Unbanned}
    | Some banned_until ->
        Peer_status.
          { trust= new_record.trust
          ; banned= Banned_status.Banned_until banned_until }
end
