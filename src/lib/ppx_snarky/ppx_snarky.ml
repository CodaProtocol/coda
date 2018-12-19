open Core_kernel
open Location
open Ppxlib
open Asttypes
open Ast_helper

let name = "snarky"

let type_name = "polymorphic"

let parse_listlike expr =
  match expr.pexp_desc with
  | Pexp_array exprs -> exprs
  | Pexp_tuple exprs -> exprs
  | _ -> failwith "Could not convert expression into a list of expressions"

let string_of_string_expr expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (str, None)) -> mkloc str expr.pexp_loc
  | _ -> failwith "Expression is not a string: cannot extract."

let parse_to_name expr =
  match expr.pexp_desc with
  (*| Pexp_ident ident -> ident*)
  | Pexp_construct (ident, None) -> ident
  | _ -> failwith "Cannot convert this type of expression to a name."

type field_info = {field_name: str; field_module: lid; var_name: str}

let loc_map x ~f = mkloc (f x.txt) x.loc

let lid_last x = loc_map x ~f:Longident.last_exn

let parse_field field =
  match parse_listlike field with
  | field_name :: field_module :: _ ->
      let field_name = string_of_string_expr field_name in
      let field_module = parse_to_name field_module in
      let var_name =
        loc_map field_module ~f:(fun lid ->
            String.uncapitalize (Longident.last_exn lid) )
      in
      {field_name; field_module; var_name}
  | _ -> failwith "Not enough info to construct field"

let polymorphic_type_stri ~loc fields_info =
  let fields =
    List.map fields_info ~f:(fun {field_name; var_name; _} ->
        Type.field ~loc field_name (Typ.var ~loc:var_name.loc var_name.txt) )
  in
  let params =
    List.map fields_info ~f:(fun {var_name; _} ->
        (Typ.var ~loc:var_name.loc var_name.txt, Invariant) )
  in
  Str.type_ Nonrecursive
    [Type.mk ~loc ~params ~kind:(Ptype_record fields) (mkloc type_name loc)]

let accessors_stri ~loc fields_info =
  Str.value ~loc Nonrecursive
    (List.map fields_info ~f:(fun {field_name; _} ->
         let field_ident = loc_map ~f:Longident.parse field_name in
         let name_pat = Pat.var field_name in
         let destr_record = Pat.record [(field_ident, name_pat)] Open in
         let accessor_fn =
           Exp.fun_ ~loc:field_name.loc Nolabel None destr_record
             (Exp.ident ~loc:field_ident.loc field_ident)
         in
         Vb.mk ~loc:field_name.loc name_pat accessor_fn ))

let include_ ~loc ?(attr = []) mod_ =
  Str.include_ ~loc {pincl_loc= loc; pincl_attributes= attr; pincl_mod= mod_}

let localize_mod base_mod mod_name name =
  mkloc (Ldot (Ldot (base_mod.txt, mod_name.txt), name)) mod_name.loc

let polymorphic_type_instance_stri ~loc mod_name fields_info =
  let typs =
    List.map fields_info ~f:(fun {field_module; _} ->
        let path = localize_mod field_module mod_name "t" in
        Typ.constr path [] )
  in
  let bound_poly = Typ.constr (mkloc (Longident.parse type_name) loc) typs in
  Str.type_ Nonrecursive [Type.mk ~manifest:bound_poly (mkloc type_name loc)]

let t_mod_instance ~loc name fields_info =
  [ Str.module_ ?loc @@ Mb.mk ?loc:name.loc name
    @@ Mod.structure ?loc:name.loc
         [polymorphic_type_instance_stri ~loc:name.loc name fields_info]
  ; include_ ~loc (Mod.ident ~loc (loc_map ~f:Longident.parse name)) ]

let snark_mod_instance ~loc name fields_info =
  [ Str.module_ @@ Mb.mk ?loc:name.loc name
    @@ Mod.structure ?loc:name.loc
         [polymorphic_type_instance_stri ~loc:name.loc name fields_info] ]

let instances_str ~loc instances_info fields_info =
  match instances_info with
  | [] -> []
  | [t_mod] -> t_mod_instance ~loc (lid_last t_mod) fields_info
  | t_mod :: snark_mod :: _ ->
      t_mod_instance ~loc (lid_last t_mod) fields_info
      @ snark_mod_instance ~loc (lid_last snark_mod) fields_info

let str_poly_record ~loc ~path:_ expr =
  match expr with
  | { pexp_desc=
        Pexp_tuple
          [ {pexp_desc= Pexp_variant ("Instances", Some instances_info); _}
          ; {pexp_desc= Pexp_variant ("Fields", Some fields_info); _} ]; _ } ->
      let instances_info =
        instances_info |> parse_listlike |> List.map ~f:parse_to_name
      in
      let fields_info = List.map ~f:parse_field (parse_listlike fields_info) in
      let polytype = polymorphic_type_stri ~loc fields_info in
      let accessors = accessors_stri ~loc fields_info in
      let instances = instances_str ~loc instances_info fields_info in
      include_ ~loc (Mod.structure ~loc (polytype :: accessors :: instances))
  | _ -> failwith "no!"

let ext =
  Extension.declare "polymorphic_record" Extension.Context.structure_item
    Ast_pattern.(single_expr_payload __)
    str_poly_record

let () =
  Driver.register_transformation name ~rules:[Context_free.Rule.extension ext]
