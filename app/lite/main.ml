open Core_kernel
open Lite_base
open Js_of_ocaml
open Virtual_dom.Vdom
module V = Verifier

module Pos = struct
  type t = {x: float; y: float}
end

let float_to_string value = Js.to_string (Js.number_of_float value) ## toString

module Svg = struct
  open Virtual_dom.Vdom

  let blue_black = "#1F2D3D"

  let main ?class_ ~width ~height cs =
    Node.svg "svg"
      ( [ Attr.create_float "width" width
        ; Attr.create_float "height" height
        ; Attr.create "xmlns" "http://www.w3.org/2000/svg" ]
      @ Option.to_list (Option.map class_ ~f:Attr.class_) )
      cs

  let line_color = blue_black

  let path points =
    let points =
      String.concat ~sep:" "
        (List.map points ~f:(fun {Pos.x; y} ->
             sprintf "%s,%s" (float_to_string x) (float_to_string y) ))
    in
    Node.svg "polyline"
      [ Attr.create "points" points
      ; Attr.create "style"
          (sprintf "fill:none;stroke:%s;stroke-width:3" line_color) ]
      []

  let rect ?radius ~color ~width ~height ~(center: Pos.t) =
    let corner_x = center.x -. (width /. 2.) in
    let corner_y = center.y -. (height /. 2.) in
    let rad_attrs =
      Option.value_map radius ~default:[] ~f:(fun r ->
          [Attr.create_float "rx" r; Attr.create_float "ry" r] )
    in
    Node.svg "rect"
      ( [ Attr.create_float "x" corner_x
        ; Attr.create_float "y" corner_y
        ; Attr.create "style" (sprintf "fill:%s" color)
        ; Attr.create_float "width" width
        ; Attr.create_float "height" height ]
      @ rad_attrs )
      []
end

let rest_server_port = 8080

let url s = sprintf "http://localhost:%d/%s" rest_server_port s

let get url on_success on_error =
  let req = XmlHttpRequest.create () in
  req ##. onerror :=
    Dom.handler (fun _e ->
        on_error (Error.of_string "get request failed") ;
        Js._false ) ;
  req ##. onload :=
    Dom.handler (fun _e ->
        ( match Js.Opt.to_option (File.CoerceTo.string req ##. response) with
        | None -> on_error (Error.of_string "get request failed")
        | Some s -> on_success (Js.to_string s) ) ;
        Js._false ) ;
  req ## _open (Js.string "GET") (Js.string url) Js._true ;
  req ## send Js.Opt.empty

let get_account _pk on_sucess on_error =
  let url = "/chain" in
  get url
    (fun s ->
      let chain = Binable.of_string (module Lite_chain) (B64.decode s) in
      on_sucess chain )
    on_error

let to_base64 m x = B64.encode (Binable.to_string m x)

module State = struct
  type t =
    { verification: [`Pending of int | `Complete of unit Or_error.t]
    ; chain: Lite_chain.t }

  let init = {verification= `Complete (Ok ()); chain= Lite_params.genesis_chain}

  let chain_length chain =
    chain.Lite_chain.protocol_state.consensus_state.length

  let should_update state (new_chain: Lite_chain.t) =
    Length.compare (chain_length new_chain) (chain_length state.chain) > 0
end

let color_of_hash (h: Pedersen.Digest.t) : string =
  let int_to_hex_char n =
    assert (0 <= n) ;
    assert (n < 16) ;
    if n < 10 then Char.of_int_exn (Char.to_int '0' + n)
    else Char.of_int_exn (Char.to_int 'A' + (n - 10))
  in
  let module N = Snarkette.Mnt6.N in
  let n = Snarkette.Mnt6.Fq.to_bigint h in
  let byte i =
    let bit ~start j =
      if N.test_bit n (start + (8 * i) + j) then 1 lsl j else 0
    in
    let char k =
      let c = bit ~start:(4 * k) in
      int_to_hex_char (c 0 lor c 1 lor c 2 lor c 3)
    in
    String.init 2 ~f:char
  in
  sprintf "#%s%s%s" (byte 0) (byte 1) (byte 2)

let map_adjacent_pairs xs ~f =
  let rec go acc = function
    | [_] | [] -> List.rev acc
    | x1 :: (x2 :: _ as xs) -> go (f x1 x2 :: acc) xs
  in
  go [] xs

let y_offset =
  let rec go acc pt n =
    if n = 0 then acc else go (pt +. acc) (pt /. 2.) (n - 1)
  in
  go 0. 0.5

let num_merkle_layers_shown = 10

let drop_top_of_tree =
  let rec layers = function
    | Sparse_ledger_lib.Sparse_ledger.Node (_, l, r) ->
        1 + max (layers l) (layers r)
    | Account _ -> 1
    | Hash _ -> 1
  in
  let rec drop n t =
    if n = 0 then t
    else
      match t with
      | Sparse_ledger_lib.Sparse_ledger.Account _ | Hash _ ->
          failwith "Cannot drop from Account or Hash"
      | Node (_, Hash _, t') | Node (_, t', Hash _) -> drop (n - 1) t'
      | Node (_, Account _, Node _) | Node (_, Node _, Account _) ->
          failwith "Accounts should only be at leaves"
      | Node (_, Account _, Account _) ->
          failwith "Only one account per tree supported"
      | Node (_, Node _, Node _) ->
          failwith "Only single account trees supported"
  in
  let desired_layers = num_merkle_layers_shown in
  fun t ->
    let n = layers t in
    if n <= desired_layers then t else drop (n - desired_layers) t

let style kvs =
  List.map ~f:(fun (k, v) -> sprintf "%s:%s" k v) kvs |> String.concat ~sep:";"

module Html = struct
  open Virtual_dom.Vdom

  let div = Node.div

  let extend_class c co =
    c ^ Option.value_map ~default:"" co ~f:(fun s -> " " ^ s)

  module Record = struct
    module Entry = struct
      type t =
        { label: string
        ; value: string
        ; class_: string option
        ; width: string option }

      let render {label; value; class_; width} =
        div
          ( Attr.class_ (extend_class "record-entry" class_)
          :: Option.to_list
               (Option.map width ~f:(fun w ->
                    Attr.create "style" (sprintf "flex-basis:%s" w) )) )
          [ div [Attr.class_ "record-entry-label"] [Node.text label]
          ; div [Attr.class_ "record-entry-value"] [Node.text value] ]

      let create ?class_ ?width label value = {label; value; class_; width}
    end

    module Row = struct
      type t = Entry.t list

      let render (t: t) =
        div [Attr.class_ "record-row"] (List.map ~f:Entry.render t)
    end

    type t = {class_: string option; attrs: Attr.t list; rows: Row.t list}

    let create ?(attrs= []) ?class_ rows = {class_; rows; attrs}

    let render {class_; rows; attrs} =
      div
        (Attr.class_ (extend_class "record" class_) :: attrs)
        (List.map rows ~f:Row.render)
  end
end

let field_to_base64 =
  Fn.compose Fold_lib.Fold.bool_t_to_string Snarkette.Mnt6.Fq.fold_bits
  |> Fn.compose B64.encode

let merkle_tree =
  let module Spec = struct
    type t =
      | Node of {pos: Pos.t; color: string}
      | Account of {pos: Pos.t; account: Account.t}

    let pos = function Node {pos; _} -> pos | Account {pos; _} -> pos
  end in
  let image_width = 500. in
  let top_offset = 30. in
  let left_offset = 300. in
  let image_height = 500. in
  let num_layers = num_merkle_layers_shown in
  let layer_height = image_height /. Int.to_float num_layers in
  let x_pos, y_pos =
    let y_uncompressed ~layer =
      top_offset +. (Int.to_float layer *. layer_height)
    in
    let x_uncompressed =
      let delta = 0.5 *. image_width /. Float.of_int num_merkle_layers_shown in
      fun ~layer ~index ->
        let rec go remaining_depth acc pt =
          if remaining_depth = 0 then acc
          else
            let b = pt mod 2 = 1 in
            let acc = if b then acc +. delta else acc -. delta in
            go (remaining_depth - 1) acc (pt / 2)
        in
        left_offset +. go layer (image_width /. 2.) index
    in
    (*
    let _x_compressed =
      fun ~layer ~index ->
        let nodes_in_layer = 1 lsl layer in
        assert (index < nodes_in_layer);
        let cell_width =
          width /. Float.of_int nodes_in_layer
        in
        let left = Float.of_int index *. cell_width in
        left +. cell_width /. 2.
    in
    let _y_compressed =
      fun ~layer ->
        top_offset +. (y_offset layer *. image_height)
    in
*)
    (x_uncompressed, y_uncompressed)
  in
  fun (tree0: ('hash, 'account) Sparse_ledger_lib.Sparse_ledger.tree) ->
    let tree0 = drop_top_of_tree tree0 in
    let specs =
      let rec go acc layer index
          (t: ('h, 'a) Sparse_ledger_lib.Sparse_ledger.tree) =
        let pos : Pos.t = {x= x_pos ~layer ~index; y= y_pos ~layer} in
        let hash_spec h = Spec.Node {pos; color= color_of_hash h} in
        let finish x = List.rev (x :: acc) in
        match t with
        | Account account -> finish (Spec.Account {account; pos})
        | Hash h -> finish (hash_spec h)
        | Node (h, Hash _, r) ->
            go (hash_spec h :: acc) (layer + 1) ((2 * index) + 1) r
        | Node (h, l, Hash _) ->
            go (hash_spec h :: acc) (layer + 1) ((2 * index) + 0) l
        | Node (_, Account _, Node _) | Node (_, Node _, Account _) ->
            failwith "Accounts should only be at leaves"
        | Node (_, Account _, Account _) ->
            failwith "Only one account per tree supported"
        | Node (_, Node _, Node _) ->
            failwith "Only single account trees supported"
      in
      go [] 0 0 tree0
    in
    let rendered =
      let edges =
        let posns =
          {Pos.x= -50.; y= top_offset} :: List.map specs ~f:Spec.pos
        in
        Svg.path posns
      in
      let nodes =
        let base_height = layer_height *. 0.8 in
        let base_width = 1.8 *. base_height in
        let f i = sqrt (Int.to_float (i + 1)) in
        List.filter_mapi specs ~f:(fun i spec ->
            let scale = 1. /. f i in
            let width = scale *. base_width in
            let height = scale *. base_height in
            match spec with
            | Spec.Node {pos; color} ->
                Some (Svg.rect ~radius:3. ~color ~center:pos ~height ~width)
            | Spec.Account _ -> None )
      in
      let account_width = 500 in
      let account =
        match List.last_exn specs with
        | Spec.Node _ -> None
        | Spec.Account
            { pos= {x; y}
            ; account= {public_key; balance; nonce; receipt_chain_hash} } ->
            let x = x -. Float.of_int (account_width / 2) in
            let record =
              let open Html.Record in
              create ~class_:"account"
                ~attrs:
                  [ Attr.create "style"
                      (style
                         [ ("width", sprintf "%dpx" account_width)
                         ; ("position", "absolute")
                         ; ("top", float_to_string y ^ "px")
                         ; ("left", float_to_string x ^ "px") ]) ]
                [ [ Entry.create "balance" ~width:"50%"
                      (Account.Balance.to_string balance)
                  ; Entry.create "nonce" ~width:"50%"
                      (Account.Nonce.to_string nonce) ]
                ; [ Entry.create "public_key" ~width:"50%"
                      (to_base64 (module Public_key.Compressed) public_key)
                  ; Entry.create "receipt_chain" ~width:"50%"
                      (field_to_base64 receipt_chain_hash) ] ]
            in
            Some (Html.Record.render record)
      in
      Node.div [Attr.class_ "merkle-tree"]
        ( Svg.main
            ~width:(image_width +. left_offset)
            ~height:(image_height +. top_offset)
            (edges :: nodes)
        :: Option.to_list account )
    in
    rendered

let state_html
    { State.verification
    ; chain=
        { protocol_state=
            {previous_state_hash; blockchain_state; consensus_state}
        ; ledger
        ; proof } } =
  let {Blockchain_state.ledger_builder_hash; ledger_hash; timestamp} =
    blockchain_state
  in
  let {Consensus_state.length; _} = consensus_state in
  let div = Node.div in
  let class_ = Attr.class_ in
  let timestamp =
    let time =
      Time.of_span_since_epoch (Time.Span.of_ms (Int64.to_float timestamp))
    in
    Time.to_string_iso8601_basic time ~zone:Time.Zone.utc
  in
  let proof_class =
    match verification with
    | `Complete (Ok _) -> "proof valid"
    | `Complete (Error _) -> "proof invalid"
    | `Pending _ -> "proof verifying"
  in
  let state_record =
    let open Html.Record in
    create ~class_:"state"
      [ [ Entry.create "previous_state_hash"
            (field_to_base64 previous_state_hash) ]
      ; [Entry.create "length" (Length.to_string length)]
      ; [Entry.create "timestamp" timestamp]
      ; [ Entry.create "staged_ledger_hash"
            (field_to_base64 ledger_builder_hash.ledger_hash) ]
      ; [Entry.create "locked_ledger_hash" (field_to_base64 ledger_hash)] ]
  in
  div [class_ "main"]
    [ div [class_ "state-with-proof"]
        [ Html.Record.render state_record
        ; Html.Record.Entry.(
            create ~class_:proof_class "blockchain_SNARK"
              (to_base64 (module Proof) proof)
            |> render) ]
    ; merkle_tree (Sparse_ledger_lib.Sparse_ledger.tree ledger) ]

let main ~render ~get_data =
  let state = ref State.init in
  let vdom = ref (render !state) in
  let elt = (Node.to_dom !vdom :> Dom.element Js.t) in
  Dom.appendChild Dom_html.document ##. body elt ;
  let update_state_and_vdom new_state =
    state := new_state ;
    let new_vdom = render new_state in
    let patch = Node.Patch.create ~previous:!vdom ~current:new_vdom in
    Node.Patch.apply patch elt |> ignore ;
    vdom := new_vdom
  in
  let verifier = Verifier.create () in
  Verifier.set_on_message verifier ~f:(fun resp ->
      match !state.verification with
      | `Pending id when id = Verifier.Response.id resp ->
          update_state_and_vdom
            { !state with
              verification= `Complete (Verifier.Response.result resp) }
      | _ -> () ) ;
  let latest_completed = ref (-1) in
  let count = ref 0 in
  let loop () =
    let id = !count in
    incr count ;
    get_data ~on_result:(fun res ->
        if id > !latest_completed then (
          latest_completed := id ;
          if State.should_update !state res then (
            update_state_and_vdom {verification= `Pending id; chain= res} ;
            Verifier.send_verify_message verifier (res, id) ) ) )
  in
  loop () ;
  Dom_html.window ## setInterval (Js.wrap_callback (fun _ -> loop ())) 5_000.

let _ =
  main
    ~get_data:(fun ~on_result ->
      get_account () (fun chain -> on_result chain) (fun _ -> ()) )
    ~render:(fun s -> state_html s)
