(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Base
open Ast
open Angstrom
open Common
open Expr

let rec combine_lists l1 l2 =
  match l1, l2 with
  | [], [] -> []
  | x :: xs, y :: ys -> (x, y) :: combine_lists xs ys
  | _, _ -> assert false (* bad, mb [] instead *)
;;

let parse_lvalues = sep_by_comma1 parse_ident
let parse_rvalues pblock = sep_by_comma1 (parse_expr pblock)

let parse_long_var_decl pblock =
  let* () = string "var" *> ws in
  let* lvalues = parse_lvalues <* ws_line in
  let* vars_type = parse_type >>| (fun t -> Some t) <|> return None in
  let* with_init = ws_line *> char '=' *> ws *> return true <|> return false in
  if not with_init
  then (
    match vars_type with
    | Some t -> return (Long_decl_no_init (t, lvalues))
    | None -> fail)
  else
    let* rvalues = parse_rvalues pblock in
    let* () = fail_if (List.is_empty lvalues) in
    match rvalues, List.length lvalues = List.length rvalues with
    | _ :: _, true ->
      return (Long_decl_mult_init (vars_type, combine_lists lvalues rvalues))
    | _ :: _, false ->
      let* () = fail_if (List.length rvalues != 1) in
      (match List.nth rvalues 0 with
       | Some (Expr_call call) -> return (Long_decl_one_init (vars_type, lvalues, call))
       | Some _ | None -> fail)
    | [], _ -> assert false
;;

let parse_short_var_decl pblock =
  let* lvalues = parse_lvalues in
  let* () = ws_line *> string ":=" *> ws in
  let* rvalues = parse_rvalues pblock in
  let* () = fail_if (List.is_empty lvalues || List.is_empty rvalues) in
  if List.length lvalues = List.length rvalues
  then return (Stmt_short_var_decl (Short_decl_mult_init (combine_lists lvalues rvalues)))
  else
    let* () = fail_if (List.length rvalues != 1) in
    match List.nth rvalues 0 with
    | Some (Expr_call call) ->
      return (Stmt_short_var_decl (Short_decl_one_init (lvalues, call)))
    | Some _ | None -> fail
;;

let parse_assign_lvalues pblock =
  let parse_lvalue =
    let rec helper acc =
      parse_index (parse_expr pblock) acc
      >>= (fun (array, index) -> helper (Lvalue_array_index (array, index)))
      <|> return acc
    in
    parse_ident >>= fun ident -> helper (Lvalue_ident ident)
  in
  sep_by_comma1 parse_lvalue
;;

let parse_assign pblock =
  let* lvalues = parse_assign_lvalues pblock in
  let* () = ws_line *> char '=' *> ws in
  let* rvalues = parse_rvalues pblock in
  let* () = fail_if (List.is_empty lvalues || List.is_empty rvalues) in
  if List.length lvalues = List.length rvalues
  then return (Stmt_assign (Assign_mult_expr (combine_lists lvalues rvalues)))
  else
    let* () = fail_if (List.length rvalues != 1) in
    match List.nth rvalues 0 with
    | Some (Expr_call call) -> return (Stmt_assign (Assign_one_expr (lvalues, call)))
    | Some _ | None -> fail
;;

let parse_incr = parse_ident <* ws_line <* string "++" >>| fun id -> Stmt_incr id
let parse_decr = parse_ident <* ws_line <* string "--" >>| fun id -> Stmt_decr id

let parse_func_call pblock =
  parse_expr pblock
  >>= function
  | Expr_call call -> return call
  | _ -> fail
;;

let parse_stmt_call pblock = parse_func_call pblock >>| fun call -> Stmt_call call

let parse_defer pblock =
  string "defer" *> ws *> parse_func_call pblock >>| fun call -> Stmt_defer call
;;

let parse_go pblock =
  string "go" *> ws *> parse_func_call pblock >>| fun call -> Stmt_go call
;;

let parse_chan_send pblock =
  let* chan = parse_ident in
  let* expr = token "<-" *> parse_expr pblock in
  return (Stmt_chan_send (chan, expr))
;;

let parse_break = string "break" *> return Stmt_break
let parse_continue = string "continue" *> return Stmt_continue

let parse_return pblock =
  string "return" *> ws_line *> sep_by_comma (parse_expr pblock)
  >>| fun expr_list -> Stmt_return expr_list
;;

let is_valid_init_and_post = function
  | Some (Stmt_short_var_decl _)
  | Some (Stmt_assign _)
  | Some (Stmt_incr _)
  | Some (Stmt_decr _)
  | Some (Stmt_call _)
  | None -> true
  | _ -> false
;;

let parse_if pstmt pblock =
  let* () = string "if" *> ws in
  let* init = pstmt >>| (fun init -> Some init) <|> return None in
  let* () = fail_if (not (is_valid_init_and_post init)) in
  let* () = parse_stmt_sep <|> return () in
  let* cond = ws *> parse_expr pblock in
  let* if_body = ws_line *> pblock <* ws_line in
  let* else_body =
    let* else_body_exists = string "else" *> ws *> return true <|> return false in
    if else_body_exists
    then
      let* else_body = pstmt in
      match else_body with
      | Stmt_if _ | Stmt_block _ -> return (Some else_body)
      | _ -> fail
    else return None
  in
  return (Stmt_if { init; cond; if_body; else_body })
;;

let parse_default_for pstmt pblock =
  let* init = pstmt >>| Option.some <|> return None in
  let ok_init = is_valid_init_and_post init in
  let* () = parse_stmt_sep in
  let* cond = parse_expr pblock >>| Option.some <|> return None in
  let* () = parse_stmt_sep in
  let* post =
    let* next_char = peek_char_fail in
    match next_char with
    | '{' -> return None
    | _ -> pstmt >>| Option.some
  in
  let ok_post = is_valid_init_and_post post in
  if not (ok_init && ok_post)
  then fail
  else
    let* body = ws_line *> pblock in
    return (Stmt_for { init; cond; post; body })
;;

let parse_for_only_cond pblock =
  let* next_char = peek_char_fail in
  let* cond =
    match next_char with
    | '{' -> return None
    | _ -> parse_expr pblock >>| Option.some
  in
  let* body = ws_line *> pblock in
  return (Stmt_for { init = None; cond; post = None; body })
;;

let parse_for_range_n pblock =
  let* () = string "range" *> ws in
  let* range = parse_expr pblock in
  let* body = ws_line *> pblock in
  return
    (Stmt_for
       { init =
           Some
             (Stmt_short_var_decl (Short_decl_mult_init [ "i", Expr_const (Const_int 0) ]))
       ; cond = Some (Expr_bin_oper (Bin_less, Expr_ident "i", range))
       ; post = Some (Stmt_incr "i")
       ; body
       })
;;

let parse_for pstmt pblock =
  string "for"
  *> ws
  *> choice
       [ parse_default_for pstmt pblock
       ; parse_for_only_cond pblock
       ; parse_for_range_n pblock
       ]
;;

let parse_range pblock =
  let* () = string "for" *> ws in
  let* idents = sep_by_comma1 parse_ident in
  let* () = fail_if (List.length idents > 2) in
  let index, element =
    match idents with
    | first :: second :: _ -> first, Some second
    | first :: _ -> first, None
    | [] -> assert false
  in
  let* variant =
    ws_line *> (string ":=" *> return Decl <|> string "=" *> return Assign <|> fail)
  in
  let* array = ws *> string "range" *> ws *> parse_expr pblock in
  let* body = ws_line *> pblock in
  return (Stmt_range { index; element; variant; array; body })
;;

let parse_stmt pblock =
  fix (fun pstmt ->
    choice
      [ (parse_long_var_decl pblock >>| fun decl -> Stmt_long_var_decl decl)
      ; parse_short_var_decl pblock
      ; parse_incr
      ; parse_decr
      ; parse_if pstmt pblock
      ; parse_chan_send pblock
      ; parse_break
      ; parse_continue
      ; parse_return pblock
      ; parse_stmt_call pblock
      ; parse_assign pblock
      ; parse_defer pblock
      ; parse_go pblock
      ; (pblock >>| fun block -> Stmt_block block)
      ; parse_for pstmt pblock
      ; parse_range pblock
      ])
;;

let parse_block : block t =
  fix (fun pblock ->
    char '{'
    *> skip_many (ws *> parse_stmt_sep *> ws)
    *> ws
    *> sep_by (many1 parse_stmt_sep) (parse_stmt pblock)
    <* skip_many (ws *> parse_stmt_sep *> ws)
    <* ws
    <* char '}')
;;
