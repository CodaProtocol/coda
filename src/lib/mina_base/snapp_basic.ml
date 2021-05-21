[%%import
"/src/config.mlh"]

open Core_kernel

[%%ifdef
consensus_mechanism]

open Snark_params.Tick
open Signature_lib

[%%endif]

let int_to_bits ~length x = List.init length ~f:(fun i -> (x lsr i) land 1 = 1)

let int_of_bits =
  List.foldi ~init:0 ~f:(fun i acc b -> if b then acc lor (1 lsl i) else acc)

module Transition = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type 'a t = {prev: 'a; next: 'a}
      [@@deriving hlist, sexp, eq, yojson, hash, compare]
    end
  end]

  let to_input {prev; next} ~f = Random_oracle_input.append (f prev) (f next)

  [%%ifdef
  consensus_mechanism]

  let typ t =
    Typ.of_hlistable [t; t] ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist
      ~value_to_hlist:to_hlist ~value_of_hlist:of_hlist

  [%%endif]
end

module Flagged_data = struct
  type ('flag, 'a) t = {flag: 'flag; data: 'a} [@@deriving hlist, fields]

  [%%ifdef
  consensus_mechanism]

  let typ flag t =
    Typ.of_hlistable [flag; t] ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist
      ~value_to_hlist:to_hlist ~value_of_hlist:of_hlist

  [%%endif]

  let to_input' {flag; data} ~flag:f ~data:d =
    Random_oracle_input.(append (f flag) (d data))
end

module Flagged_option = struct
  type ('bool, 'a) t = {is_some: 'bool; data: 'a} [@@deriving hlist, fields]

  let to_input' {is_some; data} ~f =
    Random_oracle_input.(append (bitstring [is_some]) (f data))

  let to_input {is_some; data} ~default ~f =
    let data = if is_some then data else default in
    to_input' {is_some; data} ~f

  let of_option t ~default =
    match t with
    | None ->
        {is_some= false; data= default}
    | Some data ->
        {is_some= true; data}

  let to_option {is_some; data} = Option.some_if is_some data

  [%%ifdef
  consensus_mechanism]

  let typ t =
    Typ.of_hlistable [Boolean.typ; t] ~var_to_hlist:to_hlist
      ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist ~value_of_hlist:of_hlist

  let option_typ ~default t =
    Typ.transport (typ t) ~there:(of_option ~default) ~back:to_option

  [%%endif]
end

module Set_or_keep = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type 'a t = Set of 'a | Keep
      [@@deriving sexp, eq, compare, hash, yojson]
    end
  end]

  let map t ~f = match t with Keep -> Keep | Set x -> Set (f x)

  let to_option = function Set x -> Some x | Keep -> None

  let of_option = function Some x -> Set x | None -> Keep

  let set_or_keep t x = match t with Keep -> x | Set y -> y

  [%%ifdef
  consensus_mechanism]

  module Checked : sig
    type 'a t

    val is_keep : _ t -> Boolean.var

    val is_set : _ t -> Boolean.var

    val set_or_keep :
      if_:(Boolean.var -> then_:'a -> else_:'a -> 'a) -> 'a t -> 'a -> 'a

    val data : 'a t -> 'a

    val typ :
      dummy:'a -> ('a_var, 'a) Typ.t -> ('a_var t, 'a Stable.Latest.t) Typ.t

    val to_input :
         'a t
      -> f:('a -> ('f, Boolean.var) Random_oracle_input.t)
      -> ('f, Boolean.var) Random_oracle_input.t
  end = struct
    type 'a t = (Boolean.var, 'a) Flagged_option.t

    let set_or_keep ~if_ ({is_some; data} : _ t) x =
      if_ is_some ~then_:data ~else_:x

    let data = Flagged_option.data

    let is_set = Flagged_option.is_some

    let is_keep x = Boolean.not (Flagged_option.is_some x)

    let typ ~dummy t =
      Typ.transport
        (Flagged_option.option_typ ~default:dummy t)
        ~there:to_option ~back:of_option

    let to_input (t : _ t) ~f = Flagged_option.to_input' t ~f
  end

  let typ = Checked.typ

  [%%endif]

  let to_input t ~dummy:default ~f =
    Flagged_option.to_input ~default ~f
      (Flagged_option.of_option ~default (to_option t))
end

module Or_ignore = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type 'a t = Check of 'a | Ignore
      [@@deriving sexp, eq, compare, hash, yojson]
    end
  end]

  let to_option = function Ignore -> None | Check x -> Some x

  let of_option = function None -> Ignore | Some x -> Check x

  let map x ~f = match x with Ignore -> Ignore | Check x -> Check (f x)

  [%%ifdef
  consensus_mechanism]

  module Checked : sig
    type 'a t

    val typ_implicit :
         equal:('a -> 'a -> bool)
      -> ignore:'a
      -> ('a_var, 'a) Typ.t
      -> ('a_var t, 'a Stable.Latest.t) Typ.t

    val typ_explicit :
      ignore:'a -> ('a_var, 'a) Typ.t -> ('a_var t, 'a Stable.Latest.t) Typ.t

    val to_input :
         explicit:bool
      -> 'a t
      -> f:('a -> ('f, Boolean.var) Random_oracle_input.t)
      -> ('f, Boolean.var) Random_oracle_input.t

    val check : 'a t -> f:('a -> Boolean.var) -> Boolean.var
  end = struct
    type 'a t =
      | Implicit of 'a
      | Explicit of (Boolean.var, 'a) Flagged_option.t

    let to_input ~explicit t ~f =
      match t with
      | Implicit x ->
          if explicit then failwith "Should not be explicit" else f x
      | Explicit t ->
          if explicit then Flagged_option.to_input' t ~f
          else failwith "Should not be implicit"

    let check t ~f =
      match t with
      | Implicit x ->
          f x
      | Explicit {is_some; data} ->
          Pickles.Impls.Step.Boolean.(any [not is_some; f data])

    let typ_implicit (type a a_var) ~equal ~(ignore : a) (t : (a_var, a) Typ.t)
        : (a_var t, a Stable.Latest.t) Typ.t =
      Typ.transport t
        ~there:(function Check x -> x | Ignore -> ignore)
        ~back:(fun x -> if equal x ignore then Ignore else Check x)
      |> Typ.transport_var
           ~there:(function Implicit x -> x | Explicit _ -> assert false)
           ~back:(fun x -> Implicit x)

    let typ_explicit (type a_var a) ~ignore (t : (a_var, a) Typ.t) =
      Typ.transport_var
        (Flagged_option.option_typ ~default:ignore t)
        ~there:(function Implicit _ -> assert false | Explicit t -> t)
        ~back:(fun t -> Explicit t)
      |> Typ.transport ~there:to_option ~back:of_option
  end

  let typ_implicit = Checked.typ_implicit

  let typ_explicit = Checked.typ_explicit

  [%%endif]
end

module Account_state = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Empty | Non_empty | Any
      [@@deriving sexp, eq, yojson, hash, compare, enum]

      let to_latest = Fn.id
    end
  end]

  module Encoding = struct
    type 'b t = {any: 'b; empty: 'b} [@@deriving hlist]

    let to_input {any; empty} = Random_oracle_input.bitstring [any; empty]
  end

  let encode : t -> bool Encoding.t = function
    | Empty ->
        {any= false; empty= true}
    | Non_empty ->
        {any= false; empty= false}
    | Any ->
        {any= true; empty= false}

  let decode : bool Encoding.t -> t = function
    | {any= false; empty= true} ->
        Empty
    | {any= false; empty= false} ->
        Non_empty
    | {any= true; empty= false} | {any= true; empty= true} ->
        Any

  let to_input (x : t) = Encoding.to_input (encode x)

  let check (t : t) (x : [`Empty | `Non_empty]) =
    match (t, x) with
    | Any, _ | Non_empty, `Non_empty | Empty, `Empty ->
        Ok ()
    | _ ->
        Or_error.error_string "Bad account_type"

  [%%ifdef
  consensus_mechanism]

  module Checked = struct
    open Pickles.Impls.Step

    type t = Boolean.var Encoding.t

    let to_input (t : t) = Encoding.to_input t

    let check (t : t) ~is_empty =
      Boolean.(any [t.any; t.empty && is_empty; (not t.empty) && not is_empty])
  end

  let typ : (Checked.t, t) Typ.t =
    let open Encoding in
    Typ.of_hlistable [Boolean.typ; Boolean.typ] ~var_to_hlist:to_hlist
      ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist ~value_of_hlist:of_hlist
    |> Typ.transport ~there:encode ~back:decode

  [%%endif]
end

[%%ifdef
consensus_mechanism]

module F = Pickles.Backend.Tick.Field

let invalid_public_key : Public_key.Compressed.t Lazy.t =
  let open F in
  let f x =
    let open Pickles.Backend.Tick.Inner_curve.Params in
    b + (x * (a + square x))
  in
  let rec go i : Public_key.Compressed.t =
    if not (is_square (f i)) then {x= i; is_odd= false} else go (i + one)
  in
  lazy (go zero)

[%%else]

module F = Snark_params_nonconsensus.Field

[%%endif]
