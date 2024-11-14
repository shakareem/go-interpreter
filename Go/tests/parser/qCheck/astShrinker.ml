(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open QCheck.Shrink
open QCheck.Iter
open Ast

let shrink_ident = return "a"

let rec shrink_type = function
  | (Type_int | Type_string | Type_bool) as t -> return t
  | Type_array (_, type') ->
    let* new_type = shrink_type type' in
    return (Type_array (0, new_type))
  | Type_func (arg_types, return_types) ->
    let* new_arg_types = list ~shrink:shrink_type arg_types in
    let* new_return_types = list ~shrink:shrink_type return_types in
    return (Type_func (new_arg_types, new_return_types))
  | Type_chan (chan_dir, type') ->
    let* new_type = shrink_type type' in
    return (Type_chan (chan_dir, new_type))
;;

let shrink_idents_with_types idents_and_types =
  let sh_id_and_t id_and_t =
    let _, type' = id_and_t in
    let* new_ident = shrink_ident in
    let* new_type = shrink_type type' in
    return (new_ident, new_type)
  in
  list ~shrink:sh_id_and_t idents_and_types
;;

let shrink_anon_func shblock anon_func =
  let { args; returns; body } = anon_func in
  let* new_args = shrink_idents_with_types args in
  let* new_returns =
    match returns with
    | Some (Ident_and_types idents_and_types) ->
      let* new_ident_and_types = shrink_idents_with_types idents_and_types in
      (match new_ident_and_types with
       | _ :: _ -> return (Some (Ident_and_types new_ident_and_types))
       | [] -> return None)
    | Some (Only_types types) ->
      let* new_types = list ~shrink:shrink_type types in
      (match new_types with
       | _ :: _ -> return (Some (Only_types new_types))
       | [] -> return None)
    | None -> return None
  in
  let* new_body = shblock body in
  return { args = new_args; returns = new_returns; body = new_body }
;;

let shrink_const shexpr shblock = function
  | Const_int _ -> return (Const_int 0)
  | Const_string _ -> return (Const_string "")
  | Const_array (_, type', inits) ->
    let* new_type = shrink_type type' in
    let* new_inits = list ~shrink:shexpr inits in
    return (Const_array (0, new_type, new_inits))
  | Const_func anon_func ->
    let* new_func = shrink_anon_func shblock anon_func in
    return (Const_func new_func)
;;
