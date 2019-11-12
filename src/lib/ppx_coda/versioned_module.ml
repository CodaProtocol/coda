open Core_kernel
open Ppxlib
open Versioned_util

let parse_opt = Ast_pattern.parse ~on_error:(fun () -> None)

(* TODO: Check if we need to optcomp this for 4.08 support. *)
(*
let create_attr ~loc attr_name attr_payload =
  {Parsetree.attr_name; attr_payload; attr_loc= loc}

let modify_attr_payload attr attr_payload =
  {attr with Parsetree.attr_payload}
*)
let create_attr ~loc:_ name payload = (name, payload)

let modify_attr_payload (name, _) payload = (name, payload)

let rec add_deriving ~loc attributes =
  let (module Ast_builder) = Ast_builder.make loc in
  let payload idents =
    let payload = Ast_builder.(pstr_eval (pexp_tuple idents) []) in
    PStr [payload]
  in
  match attributes with
  | [] ->
      let attr_name = mk_loc ~loc "deriving" in
      let attr_payload = payload [[%expr bin_io]; [%expr version]] in
      [create_attr ~loc attr_name attr_payload]
  | attr :: attributes -> (
      let idents =
        Ast_pattern.(attribute (string "deriving") (single_expr_payload __))
      in
      match parse_opt idents loc attr (fun l -> Some l) with
      | None ->
          attr :: add_deriving ~loc attributes
      | Some args ->
          (* Can't use [Ast_pattern] here, because [alt] doesn't suppress the
             errors raised from the [pexp_*] patterns..
          *)
          let args =
            match args.pexp_desc with Pexp_tuple args -> args | _ -> [args]
          in
          let special_version =
            Ast_pattern.(
              pexp_apply (pexp_ident (lident (string "version"))) __)
          in
          if
            List.exists args ~f:(fun arg ->
                match parse_opt special_version loc arg (fun _ -> Some ()) with
                | None ->
                    false
                | Some () ->
                    true )
          then
            (* [version] is already present, add [bin_io] and stop recursing. *)
            modify_attr_payload attr (payload ([%expr bin_io] :: args))
            :: attributes
          else
            modify_attr_payload attr
              (payload ([%expr bin_io] :: [%expr version] :: args))
            :: attributes )

let version_type version stri =
  let loc = stri.pstr_loc in
  let t, params =
    (* NOTE: Can't use [Ast_pattern] here; it rejects attributes attached to
       types..
    *)
    match stri.pstr_desc with
    | Pstr_type
        ( rec_flag
        , [({ptype_name= {txt= "t"; _}; ptype_private= Public; _} as typ)] ) ->
        let params = typ.ptype_params in
        let typ =
          { typ with
            ptype_attributes=
              add_deriving ~loc:typ.ptype_loc typ.ptype_attributes }
        in
        let t = {stri with pstr_desc= Pstr_type (rec_flag, [typ])} in
        (t, params)
    | _ ->
        (* TODO: Handle rpc types. *)
        Location.raise_errorf ~loc "Expected a single public type t."
  in
  let (module Ast_builder) = Ast_builder.make loc in
  let with_version =
    let open Ast_builder in
    let typ =
      type_declaration ~name:(Located.mk "typ") ~params ~cstrs:[]
        ~private_:Public
        ~manifest:
          (Some (ptyp_constr (Located.lident "t") (List.map ~f:fst params)))
        ~kind:Ptype_abstract
    in
    let t_deriving =
      create_attr ~loc (Located.mk "deriving") (PStr [[%stri bin_io]])
    in
    let typ =
      {typ with ptype_attributes= t_deriving :: typ.ptype_attributes}
    in
    let t =
      type_declaration ~name:(Located.mk "t") ~params ~cstrs:[]
        ~private_:Public ~manifest:None
        ~kind:
          (Ptype_record
             [ label_declaration ~name:(Located.mk "version")
                 ~mutable_:Immutable
                 ~type_:(ptyp_constr (Located.lident "int") [])
             ; label_declaration ~name:(Located.mk "t") ~mutable_:Immutable
                 ~type_:
                   (ptyp_constr (Located.lident "typ") (List.map ~f:fst params))
             ])
    in
    let t = {t with ptype_attributes= t_deriving :: t.ptype_attributes} in
    let create = [%stri let create t = {t; version= [%e eint version]}] in
    pstr_module
      (module_binding
         ~name:(Located.mk "With_version")
         ~expr:
           (pmod_structure
              [pstr_type Recursive [typ]; pstr_type Recursive [t]; create]))
  in
  let arg_names = List.mapi params ~f:(fun i _ -> sprintf "x%i" i) in
  let apply_args =
    let args =
      List.map arg_names ~f:(fun x ->
          (Nolabel, Ast_builder.(pexp_ident (Located.lident x))) )
    in
    match args with
    | [] ->
        fun ?f:_ e -> e
    | _ ->
        fun ?f e ->
          let args =
            match f with
            | None ->
                args
            | Some f ->
                List.map args ~f:(fun (lbl, x) -> (lbl, f x))
          in
          Ast_builder.(pexp_apply e args)
  in
  let fun_args e =
    List.fold_right arg_names ~init:e ~f:(fun name e ->
        Ast_builder.(pexp_fun Nolabel None (ppat_var (Located.mk name)) e) )
  in
  let mk_field fld e =
    Ast_builder.(
      pexp_field e
        (Located.mk (Ldot (Ldot (Lident "Bin_prot", "Type_class"), fld))))
  in
  let bin_io_shadows =
    [ [%stri
        let bin_read_t =
          [%e
            fun_args
              [%expr
                fun buf ~pos_ref ->
                  let With_version.{version= read_version; t} =
                    [%e apply_args [%expr With_version.bin_read_t]]
                      buf ~pos_ref
                  in
                  (* sanity check *)
                  assert (Core_kernel.Int.equal read_version version) ;
                  t]]]
    ; [%stri
        let __bin_read_t__ =
          [%e
            fun_args
              [%expr
                fun buf ~pos_ref i ->
                  let With_version.{version= read_version; t} =
                    [%e apply_args [%expr With_version.__bin_read_t__]]
                      buf ~pos_ref i
                  in
                  (* sanity check *)
                  assert (Core_kernel.Int.equal read_version version) ;
                  t]]]
    ; [%stri
        let bin_size_t =
          [%e
            fun_args
              [%expr
                fun t ->
                  With_version.create t
                  |> [%e apply_args [%expr With_version.bin_size_t]]]]]
    ; [%stri
        let bin_write_t =
          [%e
            fun_args
              [%expr
                fun buf ~pos t ->
                  With_version.create t
                  |> [%e apply_args [%expr With_version.bin_write_t]] buf ~pos]]]
    ; [%stri let bin_shape_t = With_version.bin_shape_t]
    ; [%stri
        let bin_reader_t =
          [%e
            fun_args
              [%expr
                { Bin_prot.Type_class.read=
                    [%e apply_args ~f:(mk_field "read") [%expr bin_read_t]]
                ; vtag_read=
                    [%e apply_args ~f:(mk_field "read") [%expr __bin_read_t__]]
                }]]]
    ; [%stri
        let bin_writer_t =
          [%e
            fun_args
              [%expr
                { Bin_prot.Type_class.size=
                    [%e apply_args ~f:(mk_field "size") [%expr bin_size_t]]
                ; write=
                    [%e apply_args ~f:(mk_field "write") [%expr bin_write_t]]
                }]]]
    ; [%stri
        let bin_t =
          [%e
            fun_args
              [%expr
                { Bin_prot.Type_class.shape=
                    [%e apply_args ~f:(mk_field "shape") [%expr bin_shape_t]]
                ; writer=
                    [%e apply_args ~f:(mk_field "writer") [%expr bin_writer_t]]
                ; reader=
                    [%e apply_args ~f:(mk_field "reader") [%expr bin_reader_t]]
                }]]]
    ; [%stri
        let _ =
          ( bin_read_t
          , __bin_read_t__
          , bin_size_t
          , bin_write_t
          , bin_shape_t
          , bin_reader_t
          , bin_writer_t
          , bin_t )] ]
  in
  (List.is_empty params, t :: with_version :: bin_io_shadows)

let convert_module_stri last_version stri =
  let module_pattern =
    Ast_pattern.(
      pstr_module (module_binding ~name:__' ~expr:(pmod_structure __')))
  in
  let loc = stri.pstr_loc in
  let name, str =
    Ast_pattern.parse module_pattern loc stri
      ~on_error:(fun () ->
        Location.raise_errorf ~loc
          "Expected a statement of the form `module Vn = struct ... end`." )
      (fun name str -> (name, str))
  in
  validate_module_version name.txt name.loc ;
  let version = version_of_versioned_module_name name.txt in
  Option.iter last_version ~f:(fun last_version ->
      if version = last_version then
        (* Mimic wording of the equivalent OCaml error. *)
        Location.raise_errorf ~loc
          "Multiple definition of the module name V%i." version
      else if version >= last_version then
        Location.raise_errorf ~loc
          "Versioned modules must be listed in decreasing order." ) ;
  let type_stri, str_rest =
    match str.txt with
    | [] ->
        Location.raise_errorf ~loc:str.loc
          "Expected a type declaration in this structure."
    | type_stri :: str ->
        (type_stri, str)
  in
  let should_convert, type_versioning_str = version_type version type_stri in
  (* TODO: If [should_convert] then look for [to_latest]. *)
  let open Ast_builder.Default in
  ( version
  , pstr_module ~loc
      (module_binding ~loc ~name
         ~expr:(pmod_structure ~loc:str.loc (type_versioning_str @ str_rest)))
  , should_convert )

let convert_modbody ~loc body =
  let may_convert_latest = ref None in
  let latest_version = ref None in
  let _, rev_str, convs =
    List.fold ~init:(None, [], []) body
      ~f:(fun (version, rev_str, convs) stri ->
        let version, stri, should_convert = convert_module_stri version stri in
        ( match !may_convert_latest with
        | None ->
            may_convert_latest := Some should_convert ;
            latest_version := Some version
        | Some _ ->
            () ) ;
        let convs = if should_convert then version :: convs else convs in
        (Some version, stri :: rev_str, convs) )
  in
  let (module Ast_builder) = Ast_builder.make loc in
  let rev_str =
    match !latest_version with
    | Some latest_version ->
        let open Ast_builder in
        let latest =
          pstr_module
            (module_binding ~name:(Located.mk "Latest")
               ~expr:
                 (pmod_ident (Located.lident (sprintf "V%i" latest_version))))
        in
        latest :: rev_str
    | None ->
        rev_str
  in
  let rev_str =
    match !may_convert_latest with
    | Some true ->
        let versions =
          [%stri
            (* NOTE: This will give a type error if any of the [to_latest]
               values do not convert to [Latest.t].
            *)
            let (versions :
                  (int * (Core_kernel.Bigstring.t -> Latest.t)) array) =
              [%e
                let open Ast_builder in
                pexp_array
                  (List.map convs ~f:(fun version ->
                       let version_module =
                         Longident.Lident (sprintf "V%i" version)
                       in
                       let dot x =
                         Located.mk (Longident.Ldot (version_module, x))
                       in
                       pexp_tuple
                         [ eint version
                         ; [%expr
                             fun buf ->
                               let pos_ref = ref 0 in
                               [%e pexp_ident (dot "bin_read_t")] buf ~pos_ref
                               |> [%e pexp_ident (dot "to_latest")]] ] ))]]
        in
        let convert =
          [%stri
            (** deserializes data to the latest module version's type *)
            let deserialize_binary_opt buf =
              let open Core_kernel in
              let pos_ref = ref 0 in
              (* Rely on layout, assume that the first element of the record is
           the first data in the buffer.
        *)
              let version = Bin_prot.Std.bin_read_int ~pos_ref buf in
              Array.find_map versions ~f:(fun (i, f) ->
                  if Int.equal i version then Some (f buf) else None )]
        in
        let convert_guard = [%stri let _ = deserialize_binary_opt] in
        convert_guard :: convert :: versions :: rev_str
    | _ ->
        rev_str
  in
  List.rev rev_str

let version_module ~loc ~path:_ modname modbody =
  Printexc.record_backtrace true ;
  try
    let modname = map_loc ~f:(check_modname ~loc:modname.loc) modname in
    let modbody = map_loc ~f:(convert_modbody ~loc:modbody.loc) modbody in
    let open Ast_helper in
    Str.module_ ~loc
      (Mb.mk ~loc:modname.loc modname
         (Mod.structure ~loc:modbody.loc modbody.txt))
  with exn ->
    Format.(fprintf err_formatter "%s@." (Printexc.get_backtrace ())) ;
    raise exn

(* code for module declarations in signatures 

   - add deriving bin_io, version to list of deriving items for the type "t" in versioned modules
   - add "module Latest = Vn" to Stable module
 *)

let convert_module_type_signature_item sigitem =
  match sigitem.psig_desc with
  | Psig_type
      (recflag, [({ptype_name= {txt= "t"; loc}; ptype_attributes; _} as type_)])
    ->
      let module E = Ppxlib.Ast_builder.Make (struct
        let loc = loc
      end) in
      let open E in
      let derivings, other_attrs =
        List.partition_tf ptype_attributes ~f:(fun ({txt; _}, _) ->
            String.equal txt "deriving" )
      in
      let deriving =
        match derivings with
        | [] ->
            ({txt= "deriving"; loc}, PStr [%str bin_io, version])
        | [(s, PStr [item])] ->
            let desired_derivers = ["bin_io"; "version"] in
            let item' =
              match item.pstr_desc with
              | Pstr_eval (expr, _attrs) -> (
                match expr.pexp_desc with
                | Pexp_ident {txt= Lident s; _}
                  when List.mem desired_derivers s ~equal:String.equal ->
                    [%stri bin_io, version]
                | Pexp_ident {txt= Lident _; _} ->
                    [%stri [%e expr], bin_io, version]
                | Pexp_tuple exprs ->
                    let derivers =
                      List.filter_map exprs ~f:(fun expr ->
                          match expr.pexp_desc with
                          | Pexp_ident {txt= Lident s; _}
                            when List.mem desired_derivers s
                                   ~equal:String.equal ->
                              None
                          | _ ->
                              Some expr )
                    in
                    let pexp_desc =
                      Pexp_tuple
                        ( derivers
                        @ List.map desired_derivers ~f:(fun s ->
                              pexp_ident {txt= Lident s; loc} ) )
                    in
                    let all_derivers = {expr with pexp_desc} in
                    [%stri [%e all_derivers]]
                | _ ->
                    Location.raise_errorf ~loc:item.pstr_loc
                      "Unrecognized Pstr_eval argument in deriving attribute" )
              | _ ->
                  Location.raise_errorf ~loc:item.pstr_loc
                    "Unrecognized PStr argument in deriving attribute"
            in
            (s, PStr [item'])
        | [_] ->
            (* should be unreachable *)
            Location.raise_errorf ~loc
              "Unrecognized pattern in deriving attribute"
        | _ :: _ ->
            (* should be unreachable *)
            Location.raise_errorf ~loc "Duplicate deriving attribute"
      in
      let ptype_attributes' = deriving :: other_attrs in
      let psig_desc =
        Psig_type (recflag, [{type_ with ptype_attributes= ptype_attributes'}])
      in
      {sigitem with psig_desc}
  | _ ->
      sigitem

let convert_module_type_signature signature =
  List.map signature ~f:convert_module_type_signature_item

let convert_module_type (mod_ty : module_type) =
  match mod_ty.pmty_desc with
  | Pmty_signature signature ->
      let signature' = convert_module_type_signature signature in
      {mod_ty with pmty_desc= Pmty_signature signature'}
  | _ ->
      Location.raise_errorf ~loc:mod_ty.pmty_loc
        "Expected versioned module type to be a signature"

type accum = {latest: string option; last: int option; sigitems: signature}

let convert_module_decls ~loc:_ signature =
  let init = {latest= None; last= None; sigitems= []} in
  let f {latest; last; sigitems} sigitem =
    match sigitem.psig_desc with
    | Psig_module ({pmd_name; pmd_type; _} as pmd) ->
        validate_module_version pmd_name.txt pmd_name.loc ;
        let version = version_of_versioned_module_name pmd_name.txt in
        Option.iter last ~f:(fun n ->
            if Int.equal version n then
              Location.raise_errorf ~loc:pmd_name.loc
                "Duplicate versions in versioned modules" ;
            if Int.( > ) version n then
              Location.raise_errorf ~loc:pmd_name.loc
                "Versioned modules must be listed in decreasing order" ) ;
        let latest =
          if Option.is_none latest then Some pmd_name.txt else latest
        in
        let psig_desc' =
          Psig_module {pmd with pmd_type= convert_module_type pmd_type}
        in
        let sigitem' = {sigitem with psig_desc= psig_desc'} in
        {latest; last= Some version; sigitems= sigitem' :: sigitems}
    | _ ->
        Location.raise_errorf ~loc:sigitem.psig_loc
          "Expected versioned module declaration"
  in
  List.fold signature ~init ~f

let version_module_decl ~loc ~path:_ modname signature =
  Printexc.record_backtrace true ;
  try
    let open Ast_helper in
    let modname = map_loc ~f:(check_modname ~loc:modname.loc) modname in
    let {txt= {latest; sigitems; _}; _} =
      map_loc ~f:(convert_module_decls ~loc:signature.loc) signature
    in
    let mk_module_decl name ty_desc =
      Sig.mk ~loc (Psig_module (Md.mk ~loc name (Mty.mk ~loc ty_desc)))
    in
    let signature =
      match latest with
      | None ->
          sigitems
      | Some vn ->
          let module E = Ppxlib.Ast_builder.Make (struct
            let loc = loc
          end) in
          let open E in
          let latest =
            mk_module_decl {txt= "Latest"; loc}
              (Pmty_alias {txt= Lident vn; loc})
          in
          List.rev sigitems @ [latest]
    in
    mk_module_decl modname (Pmty_signature signature)
  with exn ->
    Format.(fprintf err_formatter "%s@." (Printexc.get_backtrace ())) ;
    raise exn

let () =
  let module_ast_pattern =
    Ast_pattern.(
      pstr
        ( pstr_module (module_binding ~name:__' ~expr:(pmod_structure __'))
        ^:: nil ))
  in
  let module_extension =
    Extension.(
      declare "versioned" Context.structure_item module_ast_pattern
        version_module)
  in
  let module_decl_ast_pattern =
    Ast_pattern.(
      psig
        ( psig_module (module_declaration ~name:__' ~type_:(pmty_signature __'))
        ^:: nil ))
  in
  let module_decl_extension =
    Extension.(
      declare "versioned" Context.signature_item module_decl_ast_pattern
        version_module_decl)
  in
  let module_rule = Context_free.Rule.extension module_extension in
  let module_decl_rule = Context_free.Rule.extension module_decl_extension in
  let rules = [module_rule; module_decl_rule] in
  Driver.register_transformation "ppx_coda/versioned_module" ~rules
