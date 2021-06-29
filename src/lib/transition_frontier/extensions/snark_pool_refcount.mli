type view =
  { removed: int
  ; refcount_table: int Transaction_snark_work.Statement.Table.t
        (** Tracks the number of blocks that have each work statement in their
            scan state.
            Work is included iff it is a member of some block scan state.
        *)
  ; best_tip_table: Transaction_snark_work.Statement.Hash_set.t
        (** The set of all snark work statements present in the scan state for
            the last 10 blocks in the best chain.
        *)
  }
[@@deriving sexp]

include Intf.Extension_intf with type view := view
