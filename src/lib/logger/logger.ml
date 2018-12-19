open Core
open Async

module Level = struct
  type t = Trace | Debug | Info | Warn | Error | Faulty_peer | Fatal
  [@@deriving bin_io, sexp, compare, yojson]
end

module Attribute = struct
  type t = string * string

  let ( ^= ) k v = (k, v)
end

(* Core modules extended with to_yojson *)
module Time = struct
  include Time

  let to_yojson t = Time.to_string t |> Yojson.Safe.from_string

  let of_yojson json =
    json |> Yojson.Safe.to_string |> fun s -> Ok (Time.of_string s)
end

module Pid = struct
  include Pid

  let to_yojson t = Pid.to_string t |> Yojson.Safe.from_string

  let of_yojson json =
    json |> Yojson.Safe.to_string |> fun s -> Ok (Pid.of_string s)
end

module Message = struct
  type t =
    { attributes: (string * string) list
    ; path: string list
    ; level: Level.t
    ; pid: Pid.t
    ; host: string
    ; time: Time.t
    ; location: string option
    ; message: string }
  [@@deriving sexp, bin_io, yojson]
end

type t =
  { null: bool
  ; attributes: (string * string) list
  ; pid: Pid.t
  ; host: string
  ; path: string list }
[@@deriving bin_io]

(* flag is set on daemon startup *)
let sexp_logging : bool Set_once.t = Set_once.create ()

let set_sexp_logging b = Set_once.set_exn sexp_logging Lexing.dummy_pos b

let get_sexp_logging () = Set_once.get_exn sexp_logging Lexing.dummy_pos

let create () =
  { attributes= []
  ; null= false
  ; pid= Unix.getpid ()
  ; host= Unix.gethostname ()
  ; path= [] }

let null () =
  let l = create () in
  {l with null= true}

let log ~level ?loc ?(attrs = []) t fmt =
  ksprintf
    (fun message ->
      let m : Message.t =
        { attributes= attrs
        ; path= t.path
        ; pid= t.pid
        ; host= t.host
        ; time= Time.now ()
        ; level
        ; message
        ; location= loc }
      in
      if t.null then ifprintf stdout ""
      else
        let output =
          if get_sexp_logging () then
            (* S-expression output *)
            Sexp.to_string_mach (Message.sexp_of_t m)
          else (* JSON output *)
            Yojson.Safe.to_string (Message.to_yojson m)
        in
        printf "%s\n" output )
    fmt

let extend t attrs = {t with attributes= t.attributes @ attrs}

let child t s =
  { t with
    path= s :: t.path
  ; attributes= t.attributes @ [("module", s); (sprintf "module-%s" s, "true")]
  }

type 'a logger =
     ?loc:string
  -> ?attrs:Attribute.t list
  -> t
  -> ('a, unit, string, unit) format4
  -> 'a

let trace ?loc ?attrs t fmt = log ~level:Trace ?loc ?attrs t fmt

let debug ?loc ?attrs t fmt = log ~level:Debug ?loc ?attrs t fmt

let info ?loc ?attrs t fmt = log ~level:Info ?loc ?attrs t fmt

let warn ?loc ?attrs t fmt = log ~level:Warn ?loc ?attrs t fmt

let error ?loc ?attrs t fmt = log ~level:Error ?loc ?attrs t fmt

let fatal ?loc ?attrs t fmt = log ~level:Fatal ?loc ?attrs t fmt

let faulty_peer ?loc ?attrs t fmt = log ~level:Faulty_peer ?loc ?attrs t fmt
