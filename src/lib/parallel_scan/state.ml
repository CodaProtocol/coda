open Core_kernel
open Coda_digestif

module Job = struct
  module Stable = struct
    module V1 = struct
      module Sequence_no = struct
        module Stable = struct
          module V1 = struct
            module T = struct
              type t = int [@@deriving sexp, bin_io, version]
            end

            include T
          end

          module Latest = V1
        end

        type t = Stable.Latest.t [@@deriving sexp]
      end

      (*A merge can have zero components, one component (either the left or the right), or two components in which case there is an integer (sequence_no) representing a set of (completed)jobs in a sequence of (completed)jobs created*)
      module Merge = struct
        module Stable = struct
          module V1 = struct
            module T = struct
              type 'a t =
                | Empty
                | Lcomp of 'a
                | Rcomp of 'a
                | Bcomp of ('a * 'a * Sequence_no.Stable.V1.t)
              [@@deriving sexp, bin_io, version]
            end

            include T
          end

          module Latest = V1
        end
      end

      module T = struct
        type ('a, 'd) t =
          | Merge of 'a Merge.Stable.V1.t
          | Base of ('d * Sequence_no.Stable.V1.t) option
        [@@deriving sexp, bin_io, version]
      end

      include T
    end

    module Latest = V1
  end

  include Stable.Latest

  let gen a_gen d_gen =
    let open Quickcheck.Generator in
    let open Quickcheck.Generator.Let_syntax in
    match%map
      variant2
        (variant4 Bool.gen a_gen a_gen (tuple3 a_gen a_gen Int.gen))
        (Option.gen (tuple2 d_gen Int.gen))
    with
    | `A (`A _) -> Merge Empty
    | `A (`B a) -> Merge (Lcomp a)
    | `A (`C a) -> Merge (Rcomp a)
    | `A (`D a) -> Merge (Bcomp a)
    | `B d -> Base d

  let gen_full a_gen d_gen =
    let open Quickcheck.Generator in
    let open Quickcheck.Generator.Let_syntax in
    match%map variant2 (tuple3 a_gen a_gen Int.gen) (tuple2 d_gen Int.gen) with
    | `A (a1, a2, o) -> Merge (Bcomp (a1, a2, o))
    | `B (d, o) -> Base (Some (d, o))
end

module Completed_job = struct
  module Stable = struct
    module V1 = struct
      (* don't use version number and module registration here, because of type parameter *)
      type 'a t = Lifted of 'a | Merged of 'a [@@deriving bin_io, sexp]
    end

    module Latest = V1
  end

  include Stable.Latest
end

module Stable = struct
  module V1 = struct
    (* don't use module registration here, because of type parameters *)
    module T = struct
      type ('a, 'd) t =
        { jobs: ('a, 'd) Job.t Ring_buffer.Stable.V1.t
        ; level_pointer: int Array.t
        ; capacity: int
        ; mutable acc: int * ('a * 'd list) option sexp_opaque
        ; mutable current_data_length: int
        ; mutable base_none_pos: int option
        ; mutable recent_tree_data: 'd list sexp_opaque
        ; mutable other_trees_data: 'd list list sexp_opaque
        ; stateful_work_order: int Queue.t
        ; mutable curr_job_seq_no: int
        ; root_level: int }
      [@@deriving sexp, bin_io]

      (* TODO : wrap Array and Queue, all other types here don't need versioning *)
      let version = 1

      let __versioned__ = true
    end

    include T
  end

  module Latest = V1
end

(* bin_io omitted from deriving list intentionally *)
type ('a, 'd) t = ('a, 'd) Stable.Latest.t =
  { jobs: ('a, 'd) Job.t Ring_buffer.t
  ; level_pointer: int Array.t
  ; capacity: int
  ; mutable acc: int * ('a * 'd list) option
  ; mutable current_data_length: int
  ; mutable base_none_pos: int option
  ; mutable recent_tree_data: 'd list sexp_opaque
  ; mutable other_trees_data: 'd list list sexp_opaque
  ; stateful_work_order: int Queue.t
  ; mutable curr_job_seq_no: int
  ; root_level: int }
[@@deriving sexp]

module Hash = struct
  type t = Digestif.SHA256.t
end

(* TODO: This should really be computed iteratively *)
let hash
    { jobs
    ; acc
    ; current_data_length
    ; base_none_pos
    ; capacity
    ; level_pointer
    ; curr_job_seq_no
    ; root_level; _ } a_to_string d_to_string =
  let h = ref (Digestif.SHA256.init ()) in
  let add_string s = h := Digestif.SHA256.feed_string !h s in
  Ring_buffer.iter jobs ~f:(function
    | Base None -> add_string "Base None"
    | Base (Some (x, o)) ->
        add_string ("Base Some " ^ d_to_string x ^ " " ^ Int.to_string o)
    | Merge Empty -> add_string "Merge Empty"
    | Merge (Rcomp a) -> add_string ("Merge Rcomp " ^ a_to_string a)
    | Merge (Lcomp a) -> add_string ("Merge Lcomp " ^ a_to_string a)
    | Merge (Bcomp (a1, a2, o)) ->
        add_string
          ( "Merge Bcomp " ^ a_to_string a1 ^ " " ^ a_to_string a2 ^ " "
          ^ Int.to_string o ) ) ;
  let i, a = acc in
  let x = base_none_pos in
  add_string (Int.to_string capacity) ;
  add_string (Int.to_string i) ;
  add_string
    (Array.fold level_pointer ~init:"" ~f:(fun s a -> s ^ Int.to_string a)) ;
  ( match a with
  | None -> add_string "None"
  | Some (a, _) -> add_string (a_to_string a) ) ;
  add_string (Int.to_string current_data_length) ;
  ( match x with
  | None -> add_string "None"
  | Some a -> add_string (Int.to_string a) ) ;
  add_string (Int.to_string curr_job_seq_no) ;
  add_string (Int.to_string root_level) ;
  Digestif.SHA256.get !h

let acc s = snd s.acc

let jobs s = s.jobs

let level_pointer s = s.level_pointer

let current_data_length s = s.current_data_length

let parallelism s = (Ring_buffer.length s.jobs + 1) / 2

let base_none_pos s = s.base_none_pos

let recent_tree_data s = s.recent_tree_data

let other_trees_data s = s.other_trees_data

let stateful_work_order s = s.stateful_work_order

let curr_job_seq_no s = s.curr_job_seq_no

let root_level s = s.root_level

let copy
    { jobs
    ; acc
    ; current_data_length
    ; base_none_pos
    ; capacity
    ; level_pointer
    ; recent_tree_data
    ; other_trees_data
    ; stateful_work_order
    ; curr_job_seq_no
    ; root_level } =
  { jobs= Ring_buffer.copy jobs
  ; acc
  ; capacity
  ; current_data_length
  ; base_none_pos
  ; level_pointer= Array.copy level_pointer
  ; recent_tree_data
  ; other_trees_data
  ; stateful_work_order= Queue.copy stateful_work_order
  ; curr_job_seq_no
  ; root_level }
