open Core_kernel

type t = ..

type id = string [@@deriving equal, yojson, sexp]

let id_of_string s = s

let string_of_id s = s

type repr =
  { id: id
  ; event_name: string
  ; arguments: String.Set.t
  ; log: t -> (string * (string * Yojson.Safe.t) list) option
  ; parse: (string * Yojson.Safe.t) list -> t option }

module Registry = struct
  let reprs : repr list ref = ref []

  let register_constructor repr = reprs := repr :: !reprs
end

let parse_exn id json_pairs =
  let result =
    List.find_map !Registry.reprs ~f:(fun repr ->
        if equal_id id repr.id then
          let json_pairs =
            (* Remove additional metadata that may have been added to the log
               message.
            *)
            List.filter json_pairs ~f:(fun (field_name, _) ->
                Set.mem repr.arguments field_name )
          in
          repr.parse json_pairs
        else None )
  in
  match result with
  | Some data ->
      data
  | None ->
      failwithf "parse_exn: did not find matching parser for id %s" id ()

let log t =
  let result =
    List.find_map !Registry.reprs ~f:(fun repr ->
        Option.map (repr.log t) ~f:(fun (msg, fields) -> (msg, repr.id, fields))
    )
  in
  match result with
  | Some data ->
      data
  | None ->
      let[@warning "-3"] name =
        Obj.extension_name (Obj.extension_constructor t)
      in
      failwithf "log: did not find matching logger for %s" name ()

let register_constructor = Registry.register_constructor

let dump_registered_events () =
  List.map !Registry.reprs ~f:(fun {event_name; id; arguments; _} ->
      (event_name, id, Set.to_list arguments) )

let check_interpolations_exn ~msg_loc msg label_names =
  (* don't use Logproc_lib, which depends on C++ code
     using Interpolator_lib allows use in js_of_ocaml
     the `parse` code is the same
  *)
  match Interpolator_lib.Interpolator.parse msg with
  | Error err ->
      failwithf
        "%s\nEncountered an error while parsing the structured log message: %s"
        msg_loc err ()
  | Ok items ->
      List.iter items ~f:(function
        | `Interpolate interp
          when not (List.mem ~equal:String.equal label_names interp) ->
            failwithf
              "%s\n\
               The structured log message contains interpolation point \
               \"$%s\" which is not a field in the record"
              msg_loc interp ()
        | _ ->
            () )
