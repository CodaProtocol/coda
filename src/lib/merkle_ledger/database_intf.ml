module type S = sig
  include Base_ledger_intf.S

  val create : ?directory_name:string -> depth:int -> unit -> t

  (** create_checkpoint would create the checkpoint and open a db connection to that checkpoint *)
  val create_checkpoint : t -> directory_name:string -> unit -> t

  (** make_checkpoint would only create the checkpoint *)
  val make_checkpoint : t -> directory_name:string -> unit

  val with_ledger : depth:int -> f:(t -> 'a) -> 'a

  module For_tests : sig
    val gen_account_location :
      ledger_depth:int -> Location.t Core.Quickcheck.Generator.t
  end
end
