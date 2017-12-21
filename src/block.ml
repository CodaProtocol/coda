open Core_kernel

module Header = struct
  type t =
    { previous_header_hash : Pedersen.Digest.t
    ; body_hash            : Pedersen.Digest.t
    ; time                 : Block_time.t
    ; deltas               : Block_time.Span.t list
    }
  [@@deriving bin_io]

  let hash t =
    let buf = Bigstring.create (bin_size_t t) in
    let s = Pedersen.State.create () in
    Pedersen.State.update s buf;
    Pedersen.State.digest s
end

module Body = struct
  type t = Int64.t
  [@@deriving bin_io]
end

type t =
  { header : Header.t
  ; body   : Body.t
  }
[@@deriving bin_io]
