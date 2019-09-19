open Core_kernel
open Coda_base
open Frontier_base

module T = struct
  type t = {logger: Logger.t}

  type view =
    { new_user_commands: User_command.t list
    ; removed_user_commands: User_command.t list
    ; reorg_best_tip: bool }

  let create ~logger frontier =
    ( {logger}
    , { new_user_commands=
          Breadcrumb.user_commands (Full_frontier.root frontier)
      ; removed_user_commands= []
      ; reorg_best_tip= false } )

  (* Get the breadcrumbs that are on bc1's path but not bc2's, and vice versa.
     Ordered oldest to newest. *)
  let get_path_diff t frontier (bc1 : Breadcrumb.t) (bc2 : Breadcrumb.t) :
      Breadcrumb.t list * Breadcrumb.t list =
    let ancestor = Full_frontier.common_ancestor frontier bc1 bc2 in
    (* Find the breadcrumbs connecting bc1 and bc2, excluding bc1. Precondition:
       bc1 is an ancestor of bc2. *)
    let path_from_to bc1 bc2 =
      let rec go cursor acc =
        if Breadcrumb.equal cursor bc1 then acc
        else
          go
            (Full_frontier.find_exn frontier @@ Breadcrumb.parent_hash cursor)
            (cursor :: acc)
      in
      go bc2 []
    in
    Logger.debug t.logger ~module_:__MODULE__ ~location:__LOC__
      !"Common ancestor: %{sexp: State_hash.t}"
      ancestor ;
    ( path_from_to (Full_frontier.find_exn frontier ancestor) bc1
    , path_from_to (Full_frontier.find_exn frontier ancestor) bc2 )

  let handle_diffs t frontier diffs : view option =
    let open Diff in
    let old_best_tip = Full_frontier.best_tip frontier in
    let view, _, should_broadcast =
      List.fold diffs
        ~init:
          ( { new_user_commands= []
            ; removed_user_commands= []
            ; reorg_best_tip= false }
          , old_best_tip
          , false )
        ~f:
          (fun ( ( {new_user_commands; removed_user_commands; reorg_best_tip= _}
                 as acc )
               , old_best_tip
               , should_broadcast ) -> function
          | Lite.E.E (Best_tip_changed new_best_tip) ->
              let new_best_tip_breadcrumb =
                Full_frontier.find_exn frontier new_best_tip
              in
              let added_to_best_tip_path, removed_from_best_tip_path =
                get_path_diff t frontier new_best_tip_breadcrumb old_best_tip
              in
              Logger.debug t.logger ~module_:__MODULE__ ~location:__LOC__
                "added %d breadcrumbs and removed %d making path to new best \
                 tip"
                (List.length added_to_best_tip_path)
                (List.length removed_from_best_tip_path)
                ~metadata:
                  [ ( "new_breadcrumbs"
                    , `List
                        (List.map ~f:Breadcrumb.to_yojson
                           added_to_best_tip_path) )
                  ; ( "old_breadcrumbs"
                    , `List
                        (List.map ~f:Breadcrumb.to_yojson
                           removed_from_best_tip_path) ) ] ;
              let new_user_commands =
                List.bind added_to_best_tip_path ~f:Breadcrumb.user_commands
                @ new_user_commands
              in
              let removed_user_commands =
                List.bind removed_from_best_tip_path
                  ~f:Breadcrumb.user_commands
                @ removed_user_commands
              in
              let reorg_best_tip =
                not (List.is_empty removed_from_best_tip_path)
              in
              ( {new_user_commands; removed_user_commands; reorg_best_tip}
              , new_best_tip_breadcrumb
              , true ) | Lite.E.E (New_node (Lite _)) ->
              (acc, old_best_tip, should_broadcast)
          | Lite.E.E (Root_transitioned _) ->
              (acc, old_best_tip, should_broadcast)
          | Lite.E.E (New_node (Full _)) -> failwith "impossible" )
    in
    Option.some_if should_broadcast view
end

include T
module Broadcasted = Functor.Make_broadcasted (T)
