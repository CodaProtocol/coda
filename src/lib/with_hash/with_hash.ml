open Core_kernel

[%%versioned
module Stable = struct
  [@@@no_toplevel_latest_type]

  module V1 = struct
    type ('a, 'h) t = {data: 'a; hash: 'h}
    [@@deriving sexp, equal, compare, hash, yojson]

    let to_latest data_latest hash_latest {data; hash} =
      {data= data_latest data; hash= hash_latest hash}
  end
end]

type ('a, 'h) t = ('a, 'h) Stable.Latest.t = {data: 'a; hash: 'h}
[@@deriving sexp, equal, compare, hash, yojson]

let data {data; _} = data

let hash {hash; _} = hash

let map t ~f = {t with data= f t.data}

let of_data data ~hash_data = {data; hash= hash_data data}
