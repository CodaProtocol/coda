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

  (** potential_snarked_ledgers is a json file that stores a list of potential
      snarked ledgeres *)
  let potential_snarked_ledgers root =
    Filename.concat root "potential_snarked_ledgers.json"

  (** potential_snarked_ledger is the actual location of each potential snarked
      ledger *)
  let potential_snarked_ledger root =
    let uuid = Uuid_unix.create () in
    Filename.concat root ("snarked_ledger" ^ Uuid.to_string_hum uuid)

  let root_identifier root = Filename.concat root "root"
end

(* TODO: create a reusable singleton factory abstraction *)
module rec Instance_type : sig
  type t =
    { snarked_ledger: Ledger.Db.t
    ; potential_snarked_ledgers: string Queue.t
    ; factory: Factory_type.t }
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

  let load_potential_snarked_ledgers_from_disk factory =
    let location = Locations.potential_snarked_ledgers factory.directory in
    if phys_equal (Sys.file_exists location) `Yes then
      Yojson.Safe.from_file location |> potential_snarked_ledgers_of_yojson
    else []

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

  let destroy t =
    List.iter (Queue.to_list t.potential_snarked_ledgers) ~f:File_system.rmrf ;
    File_system.rmrf (Locations.potential_snarked_ledgers t.factory.directory) ;
    Ledger.Db.close t.snarked_ledger ;
    t.factory.instance <- None

  let close t =
    Ledger.Db.close t.snarked_ledger ;
    t.factory.instance <- None

  let create factory =
    let snarked_ledger =
      Ledger.Db.create ~depth:factory.ledger_depth
        ~directory_name:(Locations.snarked_ledger factory.directory)
        ()
    in
    {snarked_ledger; potential_snarked_ledgers= Queue.create (); factory}

  (** When we load from disk,
      1. Check the potential_snarked_ledgers to see if any one of these
         matches the snarked_ledger_hash in persistent_frontier;
      2. if none of those works, we load the old snarked_ledger and check if
         the old snarked_ledger matches with persistent_frontier;
      3. if not, we just reset all the persisted data and start from genesis
   *)
  let load_from_disk factory ~snarked_ledger_hash =
    let potential_snarked_ledgers =
      load_potential_snarked_ledgers_from_disk factory
    in
    let snarked_ledger =
      List.fold_until potential_snarked_ledgers ~init:None
        ~f:(fun _ location ->
          let potential_snarked_ledger =
            Ledger.Db.create ~depth:factory.ledger_depth
              ~directory_name:location ()
          in
          let potential_snarked_ledger_hash =
            Frozen_ledger_hash.of_ledger_hash
            @@ Ledger.Db.merkle_root potential_snarked_ledger
          in
          if
            Frozen_ledger_hash.equal potential_snarked_ledger_hash
              snarked_ledger_hash
          then (
            let snarked_ledger =
              Ledger.Db.create ~depth:factory.ledger_depth
                ~directory_name:
                  (Locations.tmp_snarked_ledger factory.directory)
                ()
            in
            match
              Ledger_transfer.transfer_accounts
                ~src:(Ledger.of_database potential_snarked_ledger)
                ~dest:snarked_ledger
            with
            | Ok _ ->
                Ledger.Db.close potential_snarked_ledger ;
                File_system.rmrf @@ Locations.snarked_ledger factory.directory ;
                Sys.rename
                  (Locations.tmp_snarked_ledger factory.directory)
                  (Locations.snarked_ledger factory.directory) ;
                List.iter potential_snarked_ledgers ~f:File_system.rmrf ;
                File_system.rmrf
                  (Locations.potential_snarked_ledgers factory.directory) ;
                Stop (Some snarked_ledger)
            | Error e ->
                Ledger.Db.close potential_snarked_ledger ;
                List.iter potential_snarked_ledgers ~f:File_system.rmrf ;
                File_system.rmrf
                  (Locations.potential_snarked_ledgers factory.directory) ;
                [%log' error factory.logger]
                  ~metadata:[("error", `String (Error.to_string_hum e))]
                  "Ledger_transfer failed" ;
                Stop None )
          else (
            Ledger.Db.close potential_snarked_ledger ;
            Continue None ) )
        ~finish:(fun _ ->
          List.iter potential_snarked_ledgers ~f:File_system.rmrf ;
          File_system.rmrf
            (Locations.potential_snarked_ledgers factory.directory) ;
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
            ; potential_snarked_ledgers= Queue.create ()
            ; factory }
        else (
          Ledger.Db.close snarked_ledger ;
          Error `Snarked_ledger_mismatch )
    | Some snarked_ledger ->
        Ok {snarked_ledger; potential_snarked_ledgers= Queue.create (); factory}

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
          ( Root_identifier.Stable.Latest.bin_write_t buf ~pos:0
              new_root_identifier
            : int ) )

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

let create_instance_exn t =
  assert (Option.is_none t.instance) ;
  let instance = Instance.create t in
  t.instance <- Some instance ;
  instance

let load_from_disk_exn t ~snarked_ledger_hash =
  let open Result.Let_syntax in
  assert (Option.is_none t.instance) ;
  let%map instance = Instance.load_from_disk t ~snarked_ledger_hash in
  t.instance <- Some instance ;
  instance

let with_instance_exn t ~f =
  let instance = create_instance_exn t in
  let x = f instance in
  Instance.close instance ; x

let reset_to_genesis_exn t ~precomputed_values =
  assert (Option.is_none t.instance) ;
  File_system.rmrf t.directory ;
  with_instance_exn t ~f:(fun instance ->
      ignore
        ( Ledger_transfer.transfer_accounts
            ~src:
              (Lazy.force
                 (Precomputed_values.genesis_ledger precomputed_values))
            ~dest:(Instance.snarked_ledger instance)
          : Ledger.Db.t Or_error.t ) ;
      Instance.set_root_identifier instance
        (genesis_root_identifier
           ~genesis_state_hash:
             (Precomputed_values.genesis_state_hash precomputed_values)) )
