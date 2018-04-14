open Core
open Async

module Location = String
type location = Location.t

include Checked_data

module Controller = struct
  type nonrec 'a t =
    { log : Logger.t
    ; tc : 'a Bin_prot.Type_class.t
    ; mem : ('a t) Location.Table.t
    }
  let create ~parent_log tc =
    { log = Logger.child parent_log "storage.with_checksum.memory"
    ; tc
    ; mem = Location.Table.create ()
    }
end

let load (c : 'a Controller.t) location =
  Deferred.return begin
    match Location.Table.find c.mem location with
    | Some t ->
      if valid c.tc t
      then Ok t.data
      else Error `Checksum_no_match
    | None -> Error `No_exist
  end

let store (c : 'a Controller.t) location data =
  Deferred.return begin
    Location.Table.set c.mem ~key:location ~data:(wrap c.tc data)
  end

