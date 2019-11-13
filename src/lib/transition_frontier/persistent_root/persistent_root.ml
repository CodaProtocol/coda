open Async_kernel
open Core
open Coda_base
open Coda_state
open Frontier_base
module Ledger_transfer = Ledger_transfer.Make (Ledger) (Ledger.Db)

let genesis_root_identifier =
  lazy
    (let open Root_identifier.Stable.Latest in
    { state_hash= With_hash.hash (Lazy.force Genesis_protocol_state.t)
    ; frontier_hash= Frontier_hash.empty })

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
    ; mutable instance: Instance_type.t option }
end =
  Factory_type

open Instance_type
open Factory_type

module Instance = struct
  type t = Instance_type.t

  let create factory =
    let snarked_ledger =
      Ledger.Db.create
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
    Logger.trace t.factory.logger ~module_:__MODULE__ ~location:__LOC__
      ~metadata:
        [ ( "root_identifier"
          , Root_identifier.Stable.Latest.to_yojson new_root_identifier ) ]
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
            Logger.trace t.factory.logger ~module_:__MODULE__ ~location:__LOC__
              ~metadata:
                [ ( "root_identifier"
                  , Root_identifier.Stable.Latest.to_yojson root_identifier )
                ]
              "Loaded persistent root identifier" ;
            Some root_identifier )

  let set_root_state_hash t state_hash =
    let root_identifier =
      load_root_identifier t
      |> Option.value ~default:(Lazy.force genesis_root_identifier)
    in
    set_root_identifier t {root_identifier with state_hash}
end

type t = Factory_type.t

let create ~logger ~directory = {directory; logger; instance= None}

let create_instance_exn t =
  assert (t.instance = None) ;
  let instance = Instance.create t in
  t.instance <- Some instance ;
  instance

let with_instance_exn t ~f =
  let instance = create_instance_exn t in
  let x = f instance in
  Instance.destroy instance ; x

let reset_to_genesis_exn t =
  let open Deferred.Let_syntax in
  assert (t.instance = None) ;
  let%map () = File_system.remove_dir t.directory in
  with_instance_exn t ~f:(fun instance ->
      ignore
        (Ledger_transfer.transfer_accounts
           ~src:(Lazy.force Genesis_ledger.t)
           ~dest:(Instance.snarked_ledger instance)) ;
      Instance.set_root_identifier instance
        (Lazy.force genesis_root_identifier) )
