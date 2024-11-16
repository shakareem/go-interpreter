(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open QCheck.Shrink
open QCheck.Iter
open Ast

let shrink_ident id =
  match id with
  | "a" -> empty
  | _ -> return "a"
;;

let rec shrink_type = function
  | Type_int | Type_string | Type_bool -> empty
  | Type_array (_, type') ->
    return type' <+> shrink_type type' >|= fun t -> Type_array (0, t)
  | Type_func (arg_types, return_types) ->
    list ~shrink:shrink_type arg_types
    >|= (fun new_arg_types -> Type_func (new_arg_types, return_types))
    <+> (list ~shrink:shrink_type return_types
         >|= fun new_return_types -> Type_func (arg_types, new_return_types))
  | Type_chan (chan_dir, type') ->
    return type' <+> shrink_type type' >|= fun t -> Type_chan (chan_dir, t)
;;

let shrink_id_and_type id_and_t =
  let ident, type' = id_and_t in
  pair (shrink_ident ident) (shrink_type type')
;;

let shrink_anon_func shblock anon_func =
  if anon_func = { args = []; returns = None; body = [] }
  then empty
  else (
    let { args; returns; body } = anon_func in
    let* new_args = list ~shrink:shrink_id_and_type args in
    let* new_returns =
      match returns with
      | Some (Ident_and_types (first, hd :: tl)) ->
        let* new_ident_and_types = list ~shrink:shrink_id_and_type (first :: hd :: tl) in
        (match new_ident_and_types with
         | hd :: tl -> return (Some (Ident_and_types (hd, tl)))
         | [] -> return None)
      | Some (Ident_and_types (pair, [])) ->
        let* new_pair = shrink_id_and_type pair in
        of_list [ None; Some (Ident_and_types (new_pair, [])) ]
      | Some (Only_types (first, hd :: tl)) ->
        let* new_types = list ~shrink:shrink_type (first :: hd :: tl) in
        (match new_types with
         | hd :: tl -> return (Some (Only_types (hd, tl)))
         | [] -> return None)
      | Some (Only_types (type', [])) ->
        let* new_type = shrink_type type' in
        of_list [ None; Some (Only_types (new_type, [])) ]
      | None -> empty
    in
    let* new_body = shblock body in
    of_list
      [ { args = new_args; returns; body }
      ; { args; returns = new_returns; body }
      ; { args; returns; body = new_body }
      ])
;;

let shrink_const shexpr shblock = function
  | Const_int num ->
    (match num with
     | 0 -> empty
     | _ -> return (Const_int 0))
  | Const_string str ->
    (match str with
     | "" -> empty
     | _ -> return (Const_string ""))
  | Const_array (_, type', inits) ->
    let* new_type = shrink_type type' in
    if (new_type, inits) = (type', [])
    then empty
    else
      let* new_inits = list ~shrink:shexpr inits in
      of_list [ Const_array (0, new_type, inits); Const_array (0, type', new_inits) ]
  | Const_func anon_func -> shrink_anon_func shblock anon_func >|= fun f -> Const_func f
;;

let shrink_func_call shexpr call =
  let func, args = call in
  pair (shexpr func) (list ~shrink:shexpr args)
;;

let rec shrink_expr shblock = function
  | Expr_ident id -> shrink_ident id >|= fun id -> Expr_ident id
  | Expr_const const ->
    shrink_const (shrink_expr shblock) shblock const >|= fun c -> Expr_const c
  | Expr_index (array, index) ->
    let* new_array = (shrink_expr shblock) array in
    let* new_index = (shrink_expr shblock) index in
    of_list [ Expr_index (new_array, index); Expr_index (array, new_index) ]
  | Expr_bin_oper (_, left, right) -> of_list [ left; right ]
  | Expr_un_oper (op, expr) ->
    let* new_expr = (shrink_expr shblock) expr >|= fun expr -> Expr_un_oper (op, expr) in
    of_list [ expr; new_expr ]
  | Expr_chan_receive expr ->
    let* new_expr = (shrink_expr shblock) expr >|= fun expr -> Expr_chan_receive expr in
    of_list [ expr; new_expr ]
  | Expr_call call ->
    shrink_func_call (shrink_expr shblock) call >|= fun call -> Expr_call call
;;

let shrink_id_with_expr shblock id_and_expr =
  let id, expr = id_and_expr in
  pair (shrink_ident id) (shrink_expr shblock expr)
;;

let shrink_type_option type' =
  match type' with
  | Some t -> return None <+> (shrink_type t >|= Option.some)
  | None -> empty
;;

let shrink_long_decl shblock = function
  | Long_decl_no_init (type', first, hd :: tl) ->
    let* new_type = shrink_type type' in
    let* new_first, new_rest =
      let* new_idents = list ~shrink:shrink_ident (first :: hd :: tl) in
      match new_idents with
      | hd :: tl -> return (hd, tl)
      | [] -> of_list [ first, []; hd, [] ]
    in
    of_list
      [ Long_decl_no_init (new_type, first, hd :: tl)
      ; Long_decl_no_init (type', new_first, new_rest)
      ]
  | Long_decl_no_init (type', first, []) ->
    let* new_type = shrink_type type' in
    let* new_id = shrink_ident first in
    of_list
      [ Long_decl_no_init (new_type, first, []); Long_decl_no_init (type', new_id, []) ]
  | Long_decl_mult_init (type', first, hd :: tl) ->
    let* new_type = shrink_type_option type' in
    let* new_first, new_rest =
      let* new_assigns = list ~shrink:(shrink_id_with_expr shblock) (first :: hd :: tl) in
      match new_assigns with
      | hd :: tl -> return (hd, tl)
      | [] -> of_list [ first, []; hd, [] ]
    in
    of_list
      [ Long_decl_mult_init (new_type, first, hd :: tl)
      ; Long_decl_mult_init (type', new_first, new_rest)
      ]
  | Long_decl_mult_init (type', first, []) ->
    let* new_type = shrink_type_option type' in
    let* new_assign = shrink_id_with_expr shblock first in
    of_list
      [ Long_decl_mult_init (new_type, first, [])
      ; Long_decl_mult_init (type', new_assign, [])
      ]
  | Long_decl_one_init (type', first, second, hd :: tl, call) ->
    let* new_type = shrink_type_option type' in
    let* new_first, new_second, new_rest =
      let* new_idents = list ~shrink:shrink_ident (first :: second :: hd :: tl) in
      match new_idents with
      | fst :: snd :: tl -> return (fst, snd, tl)
      | _ :: [] | [] -> return (first, second, [])
    in
    let* new_call = shrink_func_call (shrink_expr shblock) call in
    of_list
      [ Long_decl_one_init (new_type, first, second, hd :: tl, call)
      ; Long_decl_one_init (type', new_first, new_second, new_rest, call)
      ; Long_decl_one_init (type', first, second, hd :: tl, new_call)
      ]
  | Long_decl_one_init (type', first, second, [], call) ->
    let* new_type = shrink_type_option type' in
    let* new_call = shrink_func_call (shrink_expr shblock) call in
    of_list
      [ Long_decl_one_init (new_type, first, second, [], call)
      ; Long_decl_one_init (type', first, second, [], new_call)
      ; Long_decl_mult_init (type', (first, Expr_call call), [])
      ; Long_decl_mult_init (type', (second, Expr_call call), [])
      ]
;;

let shrink_short_decl shblock = function
  | Short_decl_mult_init (first, hd :: tl) ->
    let* new_first, new_rest =
      let* new_assigns = list ~shrink:(shrink_id_with_expr shblock) (first :: hd :: tl) in
      match new_assigns with
      | hd :: tl -> return (hd, tl)
      | [] -> of_list [ first, []; hd, [] ]
    in
    return (Short_decl_mult_init (new_first, new_rest))
  | Short_decl_mult_init (first, []) ->
    shrink_id_with_expr shblock first
    >|= fun new_pair -> Short_decl_mult_init (new_pair, [])
  | Short_decl_one_init (first, second, hd :: tl, call) ->
    let* new_first, new_second, new_rest =
      let* new_idents = list ~shrink:shrink_ident (first :: second :: hd :: tl) in
      match new_idents with
      | fst :: snd :: tl -> return (fst, snd, tl)
      | _ :: [] | [] -> return (first, second, [])
    in
    let* new_call = shrink_func_call (shrink_expr shblock) call in
    of_list
      [ Short_decl_one_init (new_first, new_second, new_rest, call)
      ; Short_decl_one_init (first, second, hd :: tl, new_call)
      ]
  | Short_decl_one_init (first, second, [], call) ->
    let* new_call = shrink_func_call (shrink_expr shblock) call in
    of_list
      [ Short_decl_one_init (first, second, [], new_call)
      ; Short_decl_mult_init ((first, Expr_call call), [])
      ; Short_decl_mult_init ((second, Expr_call call), [])
      ]
;;

let rec shrink_lvalue shblcok = function
  | Lvalue_ident id -> shrink_ident id >|= fun id -> Lvalue_ident id
  | Lvalue_array_index (array, index) ->
    let* new_array = shrink_lvalue shblcok array in
    let* new_index = shrink_expr shblcok index in
    of_list
      [ array
      ; Lvalue_array_index (new_array, index)
      ; Lvalue_array_index (array, new_index)
      ]
;;

let shrink_lvalue_with_expr shblock pair =
  let lvalue, expr = pair in
  let* new_lvalue = shrink_lvalue shblock lvalue in
  let* new_expr = shrink_expr shblock expr in
  of_list [ new_lvalue, expr; lvalue, new_expr ]
;;

let shrink_assign shblock = function
  | Assign_mult_expr (first, hd :: tl) ->
    let* new_first, new_rest =
      let* new_assigns =
        list ~shrink:(shrink_lvalue_with_expr shblock) (first :: hd :: tl)
      in
      match new_assigns with
      | hd :: tl -> return (hd, tl)
      | [] -> of_list [ first, []; hd, [] ]
    in
    return (Assign_mult_expr (new_first, new_rest))
  | Assign_mult_expr (first, []) ->
    shrink_lvalue_with_expr shblock first
    >|= fun new_pair -> Assign_mult_expr (new_pair, [])
  | Assign_one_expr (first, second, hd :: tl, call) ->
    let* new_first, new_second, new_rest =
      let* new_lvalues =
        list ~shrink:(shrink_lvalue shblock) (first :: second :: hd :: tl)
      in
      match new_lvalues with
      | fst :: snd :: tl -> return (fst, snd, tl)
      | _ :: [] | [] -> return (first, second, [])
    in
    let* new_call = shrink_func_call (shrink_expr shblock) call in
    of_list
      [ Assign_one_expr (new_first, new_second, new_rest, call)
      ; Assign_one_expr (first, second, hd :: tl, new_call)
      ]
  | Assign_one_expr (first, second, [], call) ->
    let* new_call = shrink_func_call (shrink_expr shblock) call in
    of_list
      [ Assign_one_expr (first, second, [], new_call)
      ; Assign_mult_expr ((first, Expr_call call), [])
      ; Assign_mult_expr ((second, Expr_call call), [])
      ]
;;

let shrink_chan_send shblock send =
  let chan, expr = send in
  let* new_chan = shrink_ident chan in
  let* new_expr = shrink_expr shblock expr in
  of_list [ new_chan, expr; chan, new_expr ]
;;

let shrink_if_for_init shblock = function
  | Init_decl decl -> shrink_short_decl shblock decl >|= fun decl -> Init_decl decl
  | Init_assign ass -> shrink_assign shblock ass >|= fun ass -> Init_assign ass
  | Init_call call ->
    shrink_func_call (shrink_expr shblock) call >|= fun call -> Init_call call
  | Init_decr id -> shrink_ident id >|= fun id -> Init_decr id
  | Init_incr id -> shrink_ident id >|= fun id -> Init_incr id
  | Init_send send -> shrink_chan_send shblock send >|= fun send -> Init_send send
  | Init_receive chan -> shrink_expr shblock chan >|= fun chan -> Init_receive chan
;;

let rec shrink_if shblock if' =
  let { init; cond; if_body; else_body } = if' in
  let* new_init =
    return None
    <+>
    match init with
    | Some init -> return None <+> (shrink_if_for_init shblock init >|= Option.some)
    | None -> empty
  in
  let* new_cond = shrink_expr shblock cond in
  let* new_if_body = shblock if_body in
  let* new_else_body =
    return None
    <+>
    match else_body with
    | Some (Else_block block) -> shblock block >|= fun block -> Some (Else_block block)
    | Some (Else_if if') -> shrink_if shblock if' >|= fun if' -> Some (Else_if if')
    | None -> empty
  in
  of_list
    [ { init = new_init; cond; if_body; else_body }
    ; { init; cond = new_cond; if_body; else_body }
    ; { init; cond; if_body = new_if_body; else_body }
    ; { init; cond; if_body; else_body = new_else_body }
    ]
;;

let shrink_stmt shblock = function
  | Stmt_break -> return Stmt_break
  | Stmt_continue -> return Stmt_continue
  | Stmt_incr id -> shrink_ident id >|= fun id -> Stmt_incr id
  | Stmt_decr id -> shrink_ident id >|= fun id -> Stmt_decr id
  | Stmt_long_var_decl decl ->
    shrink_long_decl shblock decl >|= fun decl -> Stmt_long_var_decl decl
  | Stmt_short_var_decl decl ->
    shrink_short_decl shblock decl >|= fun decl -> Stmt_short_var_decl decl
  | Stmt_assign ass -> shrink_assign shblock ass >|= fun ass -> Stmt_assign ass
  | Stmt_call call ->
    shrink_func_call (shrink_expr shblock) call >|= fun call -> Stmt_call call
  | Stmt_go call ->
    shrink_func_call (shrink_expr shblock) call >|= fun call -> Stmt_go call
  | Stmt_defer call ->
    shrink_func_call (shrink_expr shblock) call >|= fun call -> Stmt_defer call
  | Stmt_chan_send send ->
    shrink_chan_send shblock send >|= fun send -> Stmt_chan_send send
  | Stmt_chan_receive chan ->
    shrink_expr shblock chan >|= fun chan -> Stmt_chan_receive chan
  | Stmt_return exprs ->
    list ~shrink:(shrink_expr shblock) exprs >|= fun exprs -> Stmt_return exprs
  | Stmt_if if' -> shrink_if shblock if' >|= fun if' -> Stmt_if if'
  | Stmt_for { init; cond; post; body } ->
    let* new_init =
      return None
      <+>
      match init with
      | Some init -> return None <+> (shrink_if_for_init shblock init >|= Option.some)
      | None -> empty
    in
    let* new_cond =
      return None
      <+>
      match cond with
      | Some cond -> return None <+> (shrink_expr shblock cond >|= Option.some)
      | None -> empty
    in
    let* new_post =
      return None
      <+>
      match post with
      | Some post -> return None <+> (shrink_if_for_init shblock post >|= Option.some)
      | None -> empty
    in
    let* new_body = shblock body in
    of_list
      [ Stmt_for { init = new_init; cond; post; body }
      ; Stmt_for { init; cond = new_cond; post; body }
      ; Stmt_for { init; cond; post = new_post; body }
      ; Stmt_for { init; cond; post; body = new_body }
      ]
  | Stmt_block block -> shblock block >|= fun block -> Stmt_block block
;;

let rec shrink_block block = list ~shrink:(shrink_stmt shrink_block) block

let shrink_func_decl decl =
  let ident, args_returns_and_body = decl in
  let* new_ident = shrink_ident ident in
  let* new_args_returns_and_body = shrink_anon_func shrink_block args_returns_and_body in
  of_list [ new_ident, args_returns_and_body; ident, new_args_returns_and_body ]
;;

let shrink_top_decl = function
  | Decl_func decl -> shrink_func_decl decl >|= fun decl -> Decl_func decl
  | Decl_var decl -> shrink_long_decl shrink_block decl >|= fun decl -> Decl_var decl
;;

let shrink_file file = list ~shrink:shrink_top_decl file
