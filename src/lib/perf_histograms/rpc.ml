open Core
open Async

let decorate_dispatch ~name (dispatch : ('q, 'r) Intf.dispatch) :
    ('q, 'r) Intf.dispatch =
 fun conn q ->
  let open Deferred.Or_error.Let_syntax in
  let start = Time.now () in
  let%map r = dispatch conn q in
  let span = Time.diff (Time.now ()) start in
  Perf_histograms0.add_span ~name:(sprintf "rpc_dispatch_%s" name) span ;
  Coda_metrics.(
    Network.Rpc_latency_histogram.observe Network.rpc_latency_ms_summary
      (Time.Span.to_ms span)) ;
  Coda_metrics.(
    Gauge.set (Network.rpc_latency_ms ~name) (Time.Span.to_ms span)) ;
  r

let deocorate_impl ~name (impl : ('q, 'r, 'state) Intf.impl) :
    ('q, 'r, 'state) Intf.impl =
 fun state ~version q ->
  let open Deferred.Let_syntax in
  let start = Time.now () in
  let%map r = impl state ~version q in
  Perf_histograms0.add_span
    ~name:(sprintf "rpc_impl_%s" name)
    (Time.diff (Time.now ()) start) ;
  r

module Plain = struct
  module Extend (Rpc : Intf.Rpc.S) :
    Intf.Patched.S
    with type callee_query := Rpc.Callee.query
     and type callee_response := Rpc.Callee.response
     and type caller_query := Rpc.Caller.query
     and type caller_response := Rpc.Caller.response = struct
    include Rpc

    let dispatch_multi = dispatch_multi |> decorate_dispatch ~name

    let implement_multi ?log_not_previously_seen_version f =
      implement_multi ?log_not_previously_seen_version
        (f |> deocorate_impl ~name)
  end

  (* Since currently we are using the same version of data type for all the
     nodes in the testnet, so the sizes of decoding and encoding should be
     the same. If in the future, we have different versions, we need to make
     the metrics record sizes of decoding and encoding separately. *)
  module Decorate_bin_io (Rpc : Intf.Versioned_rpc.S) = struct
    let bin_read_response buf ~pos_ref =
      let response = Rpc.bin_read_response buf ~pos_ref in
      Coda_metrics.(
        Network.Rpc_size_histogram.observe
          (Network.rpc_size_bytes ~name:Rpc.name)
          (Rpc.bin_size_response response |> Float.of_int)) ;
      response

    let bin_reader_response =
      { Bin_prot.Type_class.read= bin_read_response
      ; vtag_read= Rpc.__bin_read_response__ }

    let bin_write_response buf ~pos response =
      Coda_metrics.(
        Network.Rpc_size_histogram.observe
          (Network.rpc_size_bytes ~name:Rpc.name)
          (Rpc.bin_size_response response |> Float.of_int)) ;
      Rpc.bin_write_response buf ~pos response

    let bin_writer_response =
      { Bin_prot.Type_class.write= bin_write_response
      ; size= Rpc.bin_size_response }

    let bin_response =
      { Bin_prot.Type_class.reader= bin_reader_response
      ; writer= bin_writer_response
      ; shape= Rpc.bin_shape_response }
  end
end
