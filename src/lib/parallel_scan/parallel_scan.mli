(**
 * [Parallel_scan] describes a signature of a solution for the following
 * problem statement:
 *
 * {e Efficiently compute a periodic scan on an infinite stream pumping at some
 * target rate, prefer maximizing throughput, then minimizing latency, then
 * minimizing size of state on an infinite core machine.}
 *
 * A periodic scan is a scan that only returns incremental results of some fold
 * every so often, and usually returns [None]. It requires a way to lift some
 * data ['d] into the space ['a] where an associative fold operation exists.
 *
 * The actual work of the scan is handled out-of-band, so here we expose an
 * interface of the intermediate state of some on-going long scan.
 *
 * Conceptually, you can imagine having a series of virtual trees where data
 * starts at the base and works its way up to the top, at which point we emit
 * the next incremental result. The implementation, for efficiency, does not
 * actually construct these trees, but {i succinctly} uses a ring buffer. The
 * state of the scan are these incomplete virtual trees.
 *
 * Specifically, the state of this scan is has the following primary operations:
 *
 * {!empty} to create the initial state
 *
 * {!update} adding raw data that will be lifted and processed later; adding
 * merges for the completed raw/merged data. This moves us closer to emitting
 * something from a tree
 *
 * {!next_jobs} to get the next work to complete from this data
 *
 *)

open Core_kernel

module Sequence_number : sig
  [%%versioned:
  module Stable : sig
    module V1 : sig
      type t = int [@@deriving sexp]
    end
  end]
end

(**Each node on the tree is viewed as a job that needs to be completed. When a job is completed, it creates a new "Todo" job and marks the old job as "Done"*)
module Job_status : sig
  [%%versioned:
  module Stable : sig
    module V1 : sig
      type t = Todo | Done [@@deriving sexp]
    end
  end]

  val to_string : t -> string
end

(**number of jobs that can be added to this tree. This number corresponding to a specific level of the tree. New jobs received is distributed across the tree based on this number. *)
module Weight : sig
  [%%versioned:
  module Stable : sig
    module V1 : sig
      type t = { base : int; merge : int } [@@deriving sexp]
    end
  end]
end

(**Base Job: Proving new transactions*)
module Base : sig
  module Record : sig
    [%%versioned:
    module Stable : sig
      module V1 : sig
        type 'base t =
          { job : 'base
          ; seq_no : Sequence_number.Stable.V1.t
          ; status : Job_status.Stable.V1.t
          }
        [@@deriving sexp]
      end
    end]
  end

  module Job : sig
    [%%versioned:
    module Stable : sig
      module V1 : sig
        type 'base t = Empty | Full of 'base Record.Stable.V1.t
        [@@deriving sexp]
      end
    end]
  end

  [%%versioned:
  module Stable : sig
    module V1 : sig
      type 'base t = Weight.Stable.V1.t * 'base Job.Stable.V1.t
      [@@deriving sexp]
    end
  end]
end

(** Merge Job: Merging two proofs*)
module Merge : sig
  module Record : sig
    [%%versioned:
    module Stable : sig
      module V1 : sig
        type 'merge t =
          { left : 'merge
          ; right : 'merge
          ; seq_no : Sequence_number.Stable.V1.t
          ; status : Job_status.Stable.V1.t
          }
        [@@deriving sexp]
      end
    end]
  end

  module Job : sig
    [%%versioned:
    module Stable : sig
      module V1 : sig
        type 'merge t =
          | Empty
          | Part of 'merge (*When only the left component of the job is available since we always complete the jobs from left to right*)
          | Full of 'merge Record.Stable.V1.t
        [@@deriving sexp]
      end
    end]
  end

  [%%versioned:
  module Stable : sig
    module V1 : sig
      type 'merge t =
        (Weight.Stable.V1.t * Weight.Stable.V1.t) * 'merge Job.Stable.V1.t
      [@@deriving sexp]
    end
  end]
end

(** An available job is an incomplete job that has enough information for one
* to process it into a completed job *)
module Available_job : sig
  type ('merge, 'base) t = Base of 'base | Merge of 'merge * 'merge
  [@@deriving sexp]
end

(**Space available and number of jobs required to enqueue data.
 first = space on the current tree and number of jobs required
 to be completed
 second = If the current-tree space is less than <max_base_jobs>
 then remaining number of slots on a new tree and the corresponding
 job count.*)
module Space_partition : sig
  [%%versioned:
  module Stable : sig
    module V1 : sig
      type t = { first : int * int; second : (int * int) option }
      [@@deriving sexp]
    end
  end]
end

module Job_view : sig
  module Extra : sig
    [%%versioned:
    module Stable : sig
      module V1 : sig
        type t =
          { seq_no : Sequence_number.Stable.V1.t
          ; status : Job_status.Stable.V1.t
          }
        [@@deriving sexp]
      end
    end]
  end

  module Node : sig
    [%%versioned:
    module Stable : sig
      module V1 : sig
        type 'a t =
          | BEmpty
          | BFull of ('a * Extra.Stable.V1.t)
          | MEmpty
          | MPart of 'a
          | MFull of ('a * 'a * Extra.Stable.V1.t)
        [@@deriving sexp]
      end
    end]
  end

  [%%versioned:
  module Stable : sig
    module V1 : sig
      type 'a t = { position : int; value : 'a Node.Stable.V1.t }
      [@@deriving sexp]
    end
  end]
end

module State : sig
  [%%versioned:
  module Stable : sig
    module V1 : sig
      type nonrec ('merge, 'base) t [@@deriving sexp]
    end
  end]

  val map : ('a1, 'a2) t -> f1:('a1 -> 'b1) -> f2:('a2 -> 'b2) -> ('b1, 'b2) t

  module Hash : sig
    type t = Digestif.SHA256.t
  end

  val hash :
    ('merge, 'base) t -> ('merge -> string) -> ('base -> string) -> Hash.t

  open Container

  module Make_foldable (M : Monad.S) : sig
    (** Effectfully fold chronologically. See {!fold_chronological} *)
    val fold_chronological_until :
         ('merge, 'base) t
      -> init:'accum
      -> f_merge:
           ('accum -> 'merge Merge.t -> ('accum, 'final) Continue_or_stop.t M.t)
      -> f_base:
           ('accum -> 'base Base.t -> ('accum, 'final) Continue_or_stop.t M.t)
      -> finish:('accum -> 'final M.t)
      -> 'final M.t
  end

  val fold_chronological :
       ('merge, 'base) t
    -> init:'accum
    -> f_merge:('accum -> 'merge Merge.t -> 'accum)
    -> f_base:('accum -> 'base Base.t -> 'accum)
    -> 'accum
end

(** The initial state of the parallel scan at some parallelism *)
val empty : max_base_jobs:int -> delay:int -> ('merge, 'base) State.t

(** Get all the available jobs *)
val all_jobs :
  ('merge, 'base) State.t -> ('merge, 'base) Available_job.t list list

(** Get all the available jobs to be done in the next update *)
val jobs_for_next_update :
  ('merge, 'base) State.t -> ('merge, 'base) Available_job.t list list

(** Get all the available jobs to be done for the given # slots to be occupied*)
val jobs_for_slots :
     ('merge, 'base) State.t
  -> slots:int
  -> ('merge, 'base) Available_job.t list list

(** Compute how much data ['d] elements we are allowed to add to the state *)
val free_space : ('merge, 'base) State.t -> int

(** Complete jobs needed at this state -- optionally emits the ['a] at the top
 * of the tree along with the ['d list] responsible for emitting the ['a]. *)
val update :
     data:'base list
  -> completed_jobs:'merge list
  -> ('merge, 'base) State.t
  -> (('merge * 'base list) option * ('merge, 'base) State.t) Or_error.t

(** The last ['a] we emitted from the top of the tree and the ['d list]
 * responsible for that ['a]. *)
val last_emitted_value : ('merge, 'base) State.t -> ('merge * 'base list) option

(** If there aren't enough slots for [max_slots] many ['d], then before
 * continuing onto the next virtual tree, split max_slots = (x,y) such that
 * x = number of slots till the end of the current tree and y = max_slots - x
 * (starts from the begining of the next tree)  *)
val partition_if_overflowing : ('merge, 'base) State.t -> Space_partition.t

(** Get the current job sequence number *)
val current_job_sequence_number : ('merge, 'base) State.t -> int

(**Each list corresponds to the jobs on one of the trees*)
val view_jobs_with_position :
     ('merge, 'base) State.t
  -> ('merge -> 'c)
  -> ('base -> 'c)
  -> 'c Job_view.t list list

(** All the base jobs that are part of the latest tree being filled
 * i.e., does not include base jobs that are part of previous trees not
 * promoted to the merge jobs yet*)
val base_jobs_on_latest_tree : ('merge, 'base) State.t -> 'base list

(** Returns true only if the next 'd that could be enqueued is
on a new tree*)
val next_on_new_tree : ('merge, 'base) State.t -> bool

(** All the 'ds (in the order in which they were added) for which scan results are yet to computed*)
val pending_data : ('merge, 'base) State.t -> 'base list

(**update tree level metrics*)
val update_metrics : ('merge, 'base) State.t -> unit Or_error.t
