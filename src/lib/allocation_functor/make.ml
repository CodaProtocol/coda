open Core_kernel

module Partial = struct
  module Bin_io (M : Intf.Input.Bin_io_intf) :
    Intf.Partial.Bin_io_intf with type t := M.t = struct
    open Bin_prot.Type_class

    let bin_size_t = M.bin_size_t

    let bin_write_t = M.bin_write_t

    let bin_read_t buf ~pos_ref =
      Table.attach_finalizer M.id (M.bin_read_t buf ~pos_ref)

    let __bin_read_t__ buf ~pos_ref i =
      Table.attach_finalizer M.id (M.__bin_read_t__ buf ~pos_ref i)

    let bin_shape_t = M.bin_shape_t

    let bin_writer_t = M.bin_writer_t

    let bin_reader_t = { read = bin_read_t; vtag_read = __bin_read_t__ }

    let bin_t =
      { shape = bin_shape_t; writer = bin_writer_t; reader = bin_reader_t }
  end

  module Sexp (M : Intf.Input.Sexp_intf) :
    Intf.Partial.Sexp_intf with type t := M.t = struct
    let sexp_of_t = M.sexp_of_t

    let t_of_sexp t = Table.attach_finalizer M.id (M.t_of_sexp t)
  end

  module Yojson (M : Intf.Input.Yojson_intf) :
    Intf.Partial.Yojson_intf with type t := M.t = struct
    let to_yojson = M.to_yojson

    let of_yojson json =
      M.of_yojson json |> Result.map ~f:(Table.attach_finalizer M.id)
  end
end

module Basic (M : Intf.Input.Basic_intf) :
  Intf.Output.Basic_intf with type t = M.t and type 'a creator := 'a M.creator =
struct
  type t = M.t

  let create = M.map_creator M.create ~f:(Table.attach_finalizer M.id)
end

module Bin_io (M : Intf.Input.Bin_io_intf) :
  Intf.Output.Bin_io_intf with type t = M.t and type 'a creator := 'a M.creator =
struct
  include Basic (M)
  include Partial.Bin_io (M)
end

module Sexp (M : Intf.Input.Sexp_intf) :
  Intf.Output.Sexp_intf with type t = M.t and type 'a creator := 'a M.creator =
struct
  include Basic (M)
  include Partial.Sexp (M)
end

module Bin_io_and_sexp (M : Intf.Input.Bin_io_and_sexp_intf) :
  Intf.Output.Bin_io_and_sexp_intf
    with type t = M.t
     and type 'a creator := 'a M.creator = struct
  include Basic (M)
  include Partial.Bin_io (M)
  include Partial.Sexp (M)
end

module Yojson (M : Intf.Input.Yojson_intf) :
  Intf.Output.Yojson_intf with type t = M.t and type 'a creator := 'a M.creator =
struct
  include Basic (M)
  include Partial.Yojson (M)
end

module Bin_io_and_yojson (M : Intf.Input.Bin_io_and_yojson_intf) :
  Intf.Output.Bin_io_and_yojson_intf
    with type t = M.t
     and type 'a creator := 'a M.creator = struct
  include Basic (M)
  include Partial.Bin_io (M)
  include Partial.Yojson (M)
end

module Full (M : Intf.Input.Full_intf) :
  Intf.Output.Full_intf with type t = M.t and type 'a creator := 'a M.creator =
struct
  include Basic (M)
  include Partial.Bin_io (M)
  include Partial.Sexp (M)
  include Partial.Yojson (M)
end

module Versioned_v1 = struct
  module Basic_intf (M : Intf.Input.Versioned_v1.Basic_intf) : sig
    include
      Intf.Output.Versioned_v1.Basic_intf
        with type Stable.V1.t = M.Stable.V1.t
         and type 'a Stable.V1.creator = 'a M.Stable.V1.creator
  end = struct
    module Stable = struct
      module V1 = struct
        include Bin_io (struct
          let id = M.id

          include M.Stable.V1
        end)

        let __versioned__ = ()

        type 'a creator = 'a M.Stable.V1.creator
      end

      module Latest = V1

      let versions = M.Stable.versions

      let bin_read_to_latest_opt = M.Stable.bin_read_to_latest_opt
    end

    type t = Stable.V1.t
  end

  module Sexp (M : Intf.Input.Versioned_v1.Sexp_intf) : sig
    include
      Intf.Output.Versioned_v1.Sexp_intf
        with type Stable.V1.t = M.Stable.V1.t
         and type 'a Stable.V1.creator = 'a M.Stable.V1.creator
  end = struct
    module Stable = struct
      module V1 = struct
        include Bin_io_and_sexp (struct
          let id = M.id

          include M.Stable.V1
        end)

        let __versioned__ = ()

        type 'a creator = 'a M.Stable.V1.creator
      end

      module Latest = V1

      let versions = M.Stable.versions

      let bin_read_to_latest_opt = M.Stable.bin_read_to_latest_opt
    end

    type t = Stable.V1.t
  end

  module Yojson (M : Intf.Input.Versioned_v1.Yojson_intf) : sig
    include
      Intf.Output.Versioned_v1.Yojson_intf
        with type Stable.V1.t = M.Stable.V1.t
         and type 'a Stable.V1.creator = 'a M.Stable.V1.creator
  end = struct
    module Stable = struct
      module V1 = struct
        include Bin_io_and_yojson (struct
          let id = M.id

          include M.Stable.V1
        end)

        let __versioned__ = ()

        type 'a creator = 'a M.Stable.V1.creator
      end

      module Latest = V1

      let versions = M.Stable.versions

      let bin_read_to_latest_opt = M.Stable.bin_read_to_latest_opt
    end

    type t = Stable.V1.t
  end

  module Full_compare_eq_hash
      (M : Intf.Input.Versioned_v1.Full_compare_eq_hash_intf) : sig
    include
      Intf.Output.Versioned_v1.Full_compare_eq_hash_intf
        with type Stable.V1.t = M.Stable.V1.t
         and type 'a Stable.V1.creator = 'a M.Stable.V1.creator
  end = struct
    module Stable = struct
      module V1 = struct
        include Full (struct
          let id = M.id

          include M.Stable.V1
        end)

        let compare = M.Stable.V1.compare

        let equal = M.Stable.V1.equal

        let hash = M.Stable.V1.hash

        let hash_fold_t = M.Stable.V1.hash_fold_t

        let __versioned__ = ()

        type 'a creator = 'a M.Stable.V1.creator
      end

      module Latest = V1

      let versions = M.Stable.versions

      let bin_read_to_latest_opt = M.Stable.bin_read_to_latest_opt
    end

    type t = Stable.V1.t

    let equal = M.equal

    let compare = M.compare

    let hash = M.hash

    let hash_fold_t = M.hash_fold_t
  end

  module Full (M : Intf.Input.Versioned_v1.Full_intf) : sig
    include
      Intf.Output.Versioned_v1.Full_intf
        with type Stable.V1.t = M.Stable.V1.t
         and type 'a Stable.V1.creator = 'a M.Stable.V1.creator
  end = struct
    module Stable = struct
      module V1 = struct
        include Full (struct
          let id = M.id

          include M.Stable.V1
        end)

        let __versioned__ = ()

        type 'a creator = 'a M.Stable.V1.creator
      end

      module Latest = V1

      let versions = M.Stable.versions

      let bin_read_to_latest_opt = M.Stable.bin_read_to_latest_opt
    end

    type t = Stable.V1.t
  end
end

module Versioned_v2 = struct
  module Sexp (M : Intf.Input.Versioned_v2.Sexp_intf) : sig
    include
      Intf.Output.Versioned_v2.Sexp_intf
        with type Stable.V2.t = M.Stable.V2.t
         and type 'a Stable.V2.creator = 'a M.Stable.V2.creator
         and type Stable.V1.t = M.Stable.V1.t
         and type 'a Stable.V1.creator = 'a M.Stable.V1.creator
  end = struct
    module Stable = struct
      module V2 = struct
        include Bin_io_and_sexp (struct
          let id = M.id

          include M.Stable.V2
        end)

        let __versioned__ = ()

        type 'a creator = 'a M.Stable.V2.creator
      end

      module V1 = struct
        include Bin_io_and_sexp (struct
          let id = M.id

          include M.Stable.V1
        end)

        let __versioned__ = ()

        type 'a creator = 'a M.Stable.V1.creator

        let to_latest = M.Stable.V1.to_latest
      end

      module Latest = V2

      let versions = M.Stable.versions

      let bin_read_to_latest_opt = M.Stable.bin_read_to_latest_opt
    end

    type t = Stable.V2.t
  end
end
