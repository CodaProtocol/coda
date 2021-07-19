open Core

module Compose = struct
  module StringMap = Map.Make (String)

  module Service = struct
    module Volume = struct
      type t =
        { type_ : string [@key "type"]; source : string; target : string }
      [@@deriving to_yojson]

      let create source target = { type_ = "bind"; source; target }
    end

    module Environment = struct
      type t = string StringMap.t

      let create = StringMap.of_alist_exn

      let to_yojson m =
        `Assoc (m |> Map.map ~f:(fun x -> `String x) |> Map.to_alist)
    end

    type t =
      { image : string
      ; volumes : Volume.t list
      ; command : string list
      ; ports : string list
      ; environment : Environment.t
      }
    [@@deriving to_yojson]
  end

  type service_map = Service.t StringMap.t

  let merge (m1 : service_map) (m2 : service_map) =
    Base.Map.merge_skewed m1 m2 ~combine:(fun ~key:_ left _ -> left)

  let service_map_to_yojson m =
    `Assoc (m |> Map.map ~f:Service.to_yojson |> Map.to_alist)

  type t = { version : string; services : service_map } [@@deriving to_yojson]
end

type t = Compose.t [@@deriving to_yojson]

let to_string = Fn.compose Yojson.Safe.pretty_to_string to_yojson
