open Core
open Mina_base
open Frontier_base
module Ledger_transfer = Ledger_transfer.Make (Ledger) (Ledger.Db)

let genesis_root_identifier ~genesis_state_hash =
  let open Root_identifier.Stable.Latest in
  {state_hash= genesis_state_hash}

let with_file ?size filename access_level ~f =
  let open Unix in
  let shared, mode =
    match access_level with
    | `Read ->
        (false, [O_RDONLY])
    | `Write ->
        (true, [O_RDWR; O_TRUNC; O_CREAT])
  in
  let fd = Unix.openfile filename ~mode in
  let buf_size =
    match size with
    | None ->
        Int64.to_int_exn Unix.(fstat fd).st_size
    | Some sz ->
        sz
  in
  let buf = Bigstring.map_file ~shared fd buf_size in
  let x = f buf in
  Bigstring.unsafe_destroy buf ;
  Unix.close fd ;
  x

module Locations = struct
  let snarked_ledger root = Filename.concat root "snarked_ledger"

  let tmp_snarked_ledger root = Filename.concat root "tmp_snarked_ledger"

  let potential_snarked_ledgers root =
    Filename.concat root "potential_snarked_ledgers.json"

  let potential_snarked_ledger root =
    let uuid = Uuid_unix.create () in
    Filename.concat root ("snarked_ledger" ^ Uuid.to_string_hum uuid)

  let root_identifier root = Filename.concat root "root"
end

(* TODO: create a reusable singleton factory abstraction *)
module rec Instance_type : sig
  type t =
    { snarked_ledger: Ledger.Db.t
    ; factory: Factory_type.t
    ; potential_snarked_ledgers: string Queue.t }
end =
  Instance_type

and Factory_type : sig
  type t =
    { directory: string
    ; logger: Logger.t
    ; mutable instance: Instance_type.t option
    ; ledger_depth: int }
end =
  Factory_type

open Instance_type
open Factory_type

module Instance = struct
  type t = Instance_type.t

  let potential_snarked_ledgers_to_yojson queue =
    `List
      (List.map (Queue.to_list queue) ~f:(fun filename -> `String filename))

  let potential_snarked_ledgers_of_yojson json =
    Yojson.Safe.Util.to_list json |> List.map ~f:Yojson.Safe.Util.to_string

  let write_potential_snarked_ledgers_to_disk t =
    Yojson.Safe.to_file
      (Locations.potential_snarked_ledgers t.factory.directory)
      (potential_snarked_ledgers_to_yojson t.potential_snarked_ledgers)

  let enqueue_snarked_ledger ~location t =
    Queue.enqueue t.potential_snarked_ledgers location ;
    write_potential_snarked_ledgers_to_disk t

  let dequeue_snarked_ledger t =
    let location = Queue.dequeue_exn t.potential_snarked_ledgers in
    File_system.rmrf location ;
    write_potential_snarked_ledgers_to_disk t

  let destroy_potential_snarked_ledgers potential_snarked_ledgers =
    List.iter potential_snarked_ledgers ~f:File_system.rmrf

  let destroy t =
    let potential_snarked_ledger_filenames_location =
      Locations.potential_snarked_ledgers t.factory.directory
    in
    let () =
      if Sys.file_exists potential_snarked_ledger_filenames_location = `Yes
      then
        let json =
          Yojson.Safe.from_file potential_snarked_ledger_filenames_location
        in
        let potential_snarked_ledgers =
          potential_snarked_ledgers_of_yojson json
        in
        destroy_potential_snarked_ledgers potential_snarked_ledgers
      else ()
    in
    File_system.rmrf (Locations.potential_snarked_ledgers t.factory.directory) ;
    Ledger.Db.close t.snarked_ledger ;
    t.factory.instance <- None

  let create factory =
    let snarked_ledger =
      Ledger.Db.create ~depth:factory.ledger_depth
        ~directory_name:(Locations.snarked_ledger factory.directory)
        ()
    in
    {snarked_ledger; factory; potential_snarked_ledgers= Queue.create ()}

  (** When we load from disk,
      1. check the potential_snarked_ledgers to see if any one of those matches the one in persistent_frontier;
      2. if none of those works, we load the old snarked_ledger and check if the old snarked_ledger matchees with the persistent_frontier
      3. if not, we just reset all the persisted data and start from genesis 
  *)
  let load_from_disk factory ~snarked_ledger_hash =
    let potential_snarked_ledger_filenames_location =
      Locations.potential_snarked_ledgers factory.directory
    in
    let potential_snarked_ledgers =
      if Sys.file_exists potential_snarked_ledger_filenames_location = `Yes
      then
        let json =
          Yojson.Safe.from_file potential_snarked_ledger_filenames_location
        in
        potential_snarked_ledgers_of_yojson json
      else []
    in
    let snarked_ledger =
      List.fold_until potential_snarked_ledgers ~init:None
        ~f:(fun _ location ->
          let ledger =
            Ledger.Db.create ~depth:factory.ledger_depth
              ~directory_name:location ()
          in
          let potential_snarked_ledger_hash =
            Frozen_ledger_hash.of_ledger_hash @@ Ledger.Db.merkle_root ledger
          in
          if
            Frozen_ledger_hash.equal potential_snarked_ledger_hash
              snarked_ledger_hash
          then (
            (* Here I first create an empty database and then transfer the data from
               checkpoint to the empty database. Then remove the old snarked_ledger and
               rename the newly created database to the actual snarked_ledger. This is
               because checkpoints are kind of symblic link, I need first copy the content
               to the new database and then I can remove the orignal one. *)
            let snarked_ledger =
              Ledger.Db.create ~depth:factory.ledger_depth
                ~directory_name:
                  (Locations.tmp_snarked_ledger factory.directory)
                ()
            in
            ignore
            @@ Ledger_transfer.transfer_accounts
                 ~src:(Ledger.of_database ledger)
                 ~dest:snarked_ledger ;
            Ledger.Db.close ledger ;
            File_system.rmrf @@ Locations.snarked_ledger factory.directory ;
            Sys.rename
              (Locations.tmp_snarked_ledger factory.directory)
              (Locations.snarked_ledger factory.directory) ;
            destroy_potential_snarked_ledgers potential_snarked_ledgers ;
            File_system.rmrf location ;
            Stop (Some snarked_ledger) )
          else Continue None )
        ~finish:(fun _ ->
          destroy_potential_snarked_ledgers potential_snarked_ledgers ;
          None )
    in
    match snarked_ledger with
    | None ->
        let snarked_ledger =
          Ledger.Db.create ~depth:factory.ledger_depth
            ~directory_name:(Locations.snarked_ledger factory.directory)
            ()
        in
        let potential_snarked_ledger_hash =
          Frozen_ledger_hash.of_ledger_hash
          @@ Ledger.Db.merkle_root snarked_ledger
        in
        if
          Frozen_ledger_hash.equal potential_snarked_ledger_hash
            snarked_ledger_hash
        then
          Ok
            { snarked_ledger
            ; factory
            ; potential_snarked_ledgers= Queue.create () }
        else (
          Ledger.Db.close snarked_ledger ;
          Error `Snarked_ledger_mismatch )
    | Some snarked_ledger ->
        Ok {snarked_ledger; factory; potential_snarked_ledgers= Queue.create ()}

  let close t =
    Ledger.Db.close t.snarked_ledger ;
    t.factory.instance <- None

  (* TODO: encapsulate functionality of snarked ledger *)
  let snarked_ledger {snarked_ledger; _} = snarked_ledger

  let set_root_identifier t new_root_identifier =
    [%log' trace t.factory.logger]
      ~metadata:
        [("root_identifier", Root_identifier.to_yojson new_root_identifier)]
      "Setting persistent root identifier" ;
    let size = Root_identifier.Stable.Latest.bin_size_t new_root_identifier in
    with_file (Locations.root_identifier t.factory.directory) `Write ~size
      ~f:(fun buf ->
        ignore
          (Root_identifier.Stable.Latest.bin_write_t buf ~pos:0
             new_root_identifier) )

  (* defaults to genesis *)
  let load_root_identifier t =
    let file = Locations.root_identifier t.factory.directory in
    match Unix.access file [`Exists; `Read] with
    | Error _ ->
        None
    | Ok () ->
        with_file file `Read ~f:(fun buf ->
            let root_identifier =
              Root_identifier.Stable.Latest.bin_read_t buf ~pos_ref:(ref 0)
            in
            [%log' trace t.factory.logger]
              ~metadata:
                [("root_identifier", Root_identifier.to_yojson root_identifier)]
              "Loaded persistent root identifier" ;
            Some root_identifier )

  let set_root_state_hash t state_hash = set_root_identifier t {state_hash}
end

type t = Factory_type.t

let create ~logger ~directory ~ledger_depth =
  {directory; logger; instance= None; ledger_depth}

let load_from_disk_exn t ~snarked_ledger_hash =
  let open Result.Let_syntax in
  assert (t.instance = None) ;
  let%map instance = Instance.load_from_disk t ~snarked_ledger_hash in
  t.instance <- Some instance ;
  instance

let create_instance_exn t =
  assert (t.instance = None) ;
  let instance = Instance.create t in
  t.instance <- Some instance ;
  instance

let with_instance_exn t ~f =
  let instance = create_instance_exn t in
  let x = f instance in
  Instance.close instance ; x

let reset_to_genesis_exn t ~precomputed_values =
  assert (t.instance = None) ;
  File_system.rmrf t.directory ;
  with_instance_exn t ~f:(fun instance ->
      ignore
        (Ledger_transfer.transfer_accounts
           ~src:
             (Lazy.force (Precomputed_values.genesis_ledger precomputed_values))
           ~dest:(Instance.snarked_ledger instance)) ;
      Instance.set_root_identifier instance
        (genesis_root_identifier
           ~genesis_state_hash:
             (Precomputed_values.genesis_state_hash precomputed_values)) )
