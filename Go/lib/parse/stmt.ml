(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Angstrom
open Common

let parse_var_decl = return () (* TODO *)
let parse_assign = return () (* TODO *)
let parse_incr = parse_ident <* string "++" >>| fun id -> Stmt_incr id
let parse_decr = parse_ident <* string "--" >>| fun id -> Stmt_decr id
let parse_if = return () (* TODO *)
let parse_for = return () (* TODO *)
let parse_range = return () (* TODO *)
let parse_break = string "break" *> return Stmt_break
let parse_continue = string "continue" *> return Stmt_continue
let parse_return = string "return" *> ws_line *> return () (* заглушка, надо parse_expr *)
let parse_chan_send = return () (* TODO *)
let parse_chan_receive = return () (* TODO *)
let parse_stmt_call = return () (* TODO *)
let parse_defer = string "defer" *> ws_line *> return () (* заглушка *)
let parse_go = string "go" *> ws_line *> return () (* заглушка *)

let parse_stmt =
  choice
    [ parse_var_decl
    ; parse_assign
    ; parse_incr *> return () (* заглушка *)
    ; parse_decr *> return () (* заглушка *)
    ; parse_if
    ; parse_for
    ; parse_range
    ; parse_break *> return () (* заглушка *)
    ; parse_continue *> return () (* заглушка *)
    ; parse_return
    ; parse_chan_send
    ; parse_chan_receive
    ; parse_stmt_call
    ; parse_defer
    ; parse_go
    ]
    ~failure_msg:"Incorrect statement"
  *> return Stmt_break (* заглушка *)
;;

let rec parse_block : block t =
  let parse_stmts = many_sep ~sep:(char ',') ~parser:parse_stmt in
  char '{' *> ws *> parse_stmts <* ws <* char '}'
;;
