open Async_kernel
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

  let root_identifier root = Filename.concat root "root"
end

(* TODO: create a reusable singleton factory abstraction *)
module rec Instance_type : sig
  type t = {snarked_ledger: Ledger.Db.t; factory: Factory_type.t}
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

  let create factory =
    let snarked_ledger =
      Ledger.Db.create ~depth:factory.ledger_depth
        ~directory_name:(Locations.snarked_ledger factory.directory)
        ()
    in
    {snarked_ledger; factory}

  let destroy t =
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

let with_instance_exn t ~f =
  let instance = create_instance_exn t in
  let x = f instance in
  Instance.destroy instance ; x

let reset_to_genesis_exn t ~precomputed_values =
  let open Deferred.Let_syntax in
  assert (Option.is_none t.instance) ;
  let%map () = File_system.remove_dir t.directory in
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
