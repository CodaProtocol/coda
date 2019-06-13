open Core
open Async

module Stable = struct
  module V1 = struct
    module T = struct
      type 'a t = [`One of 'a | `Two of 'a * 'a]
      [@@deriving bin_io, equal, compare, hash, sexp, version, yojson]
    end

    include T
  end

  module Latest = V1
end

type 'a t = 'a Stable.V1.t [@@deriving compare, equal, hash, sexp, yojson]

let length = function `One _ -> 1 | `Two _ -> 2

let to_list = function `One a -> [a] | `Two (a, b) -> [a; b]

let group_sequence : 'a Sequence.t -> 'a t Sequence.t =
 fun to_group ->
  Sequence.unfold ~init:to_group ~f:(fun acc ->
      match Sequence.next acc with
      | None ->
          None
      | Some (a, rest_1) -> (
        match Sequence.next rest_1 with
        | None ->
            Some (`One a, Sequence.empty)
        | Some (b, rest_2) ->
            Some (`Two (a, b), rest_2) ) )

let group_list : 'a list -> 'a t list =
 fun xs -> xs |> Sequence.of_list |> group_sequence |> Sequence.to_list

let zip_exn : 'a t -> 'b t -> ('a * 'b) t =
 fun a b ->
  match (a, b) with
  | `One a1, `One b1 ->
      `One (a1, b1)
  | `Two (a1, a2), `Two (b1, b2) ->
      `Two ((a1, b1), (a2, b2))
  | _ ->
      failwith "One_or_two.zip_exn mismatched"

module Monadic (M : Monad.S) : Intfs.Monadic with type 'a m := 'a M.t = struct
  let sequence : 'a M.t t -> 'a t M.t = function
    | `One def ->
        M.map def ~f:(fun x -> `One x)
    | `Two (def1, def2) ->
        let open M.Let_syntax in
        let%bind a = def1 in
        let%map b = def2 in
        `Two (a, b)

  let map : 'a t -> f:('a -> 'b M.t) -> 'b t M.t =
   fun t ~f ->
    sequence
    @@ match t with `One a -> `One (f a) | `Two (a, b) -> `Two (f a, f b)

  let fold :
      'a t -> init:'accum -> f:('accum -> 'a -> 'accum M.t) -> 'accum M.t =
   fun t ~init ~f ->
    match t with
    | `One a ->
        f init a
    | `Two (a, b) ->
        M.bind (f init a) ~f:(fun x -> f x b)
end

module Ident = Monadic (Monad.Ident)
module Deferred = Monadic (Deferred)
module Option = Monadic (Option)
module Or_error = Monadic (Or_error)

let map = Ident.map

let fold = Ident.fold

let iter t ~f = match t with `One a -> f a | `Two (a, b) -> f a ; f b

let fold_until ~init ~f ~finish t =
  Container.fold_until ~fold ~init ~f ~finish t

let gen inner_gen =
  Quickcheck.Generator.(
    union
      [ map inner_gen ~f:(fun x -> `One x)
      ; map (tuple2 inner_gen inner_gen) ~f:(fun pair -> `Two pair) ])
