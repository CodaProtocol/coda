open Async
open Signature_lib

type t

val load : logger:Logger.t -> disk_location:string -> t Deferred.t

val reload : logger:Logger.t -> t -> unit Deferred.t

val import_keypair :
     t
  -> Keypair.t
  -> password:Secret_file.password
  -> Public_key.Compressed.t Deferred.t

val import_keypair_terminal_stdin :
  t -> Keypair.t -> Public_key.Compressed.t Deferred.t

val generate_new :
  t -> password:Secret_file.password -> Public_key.Compressed.t Deferred.t

val create_hd_account :
     t
  -> hd_index:Mina_numbers.Hd_index.t
  -> (Public_key.Compressed.t, string) Deferred.Result.t

val pks : t -> Public_key.Compressed.t list

val find_unlocked : t -> needle:Public_key.Compressed.t -> Keypair.t option

val find_identity :
     t
  -> needle:Public_key.Compressed.t
  -> [`Keypair of Keypair.t | `Hd_index of Mina_numbers.Hd_index.t] option

val check_locked : t -> needle:Public_key.Compressed.t -> bool option

val unlock :
     t
  -> needle:Public_key.Compressed.t
  -> password:Secret_file.password
  -> (unit, [`Not_found | `Bad_password]) Deferred.Result.t

val lock : t -> needle:Public_key.Compressed.t -> unit

val get_path : t -> Public_key.Compressed.t -> string

val delete :
  t -> Public_key.Compressed.t -> (unit, [`Not_found]) Deferred.Result.t

val get_tracked_keypair :
     logger:Logger.t
  -> which:string
  -> read_from_env_exn:(   logger:Logger.t
                        -> which:string
                        -> string
                        -> Keypair.t Deferred.t)
  -> conf_dir:string
  -> Public_key.Compressed.t
  -> Keypair.t Deferred.t
