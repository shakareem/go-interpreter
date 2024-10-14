(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Angstrom
open Common
open Expr
open Stmt

let parse_var_decl_in_func = return () (* TODO *)
let parse_var_decl_any = parse_var_decl_top_level *> return () <|> parse_var_decl_in_func
let parse_assign = return () (* TODO *)
let parse_incr = parse_ident <* string "++" >>| fun id -> Stmt_incr id
let parse_decr = parse_ident <* string "--" >>| fun id -> Stmt_decr id

(* в ините нужно парсить только parse_var_decl_in_func *)
let parse_if =
  let parse_init =
    parse_stmt <* parse_stmt_sep >>| (fun init -> Some init) <|> return None
  in
  let parse_else =
    string "else" *> ws *> parse_block >>| (fun block -> Some block) <|> return None
  in
  string "if"
  *> ws
  *> lift4
       (fun init cond if_body else_body -> Stmt_if (init, cond, if_body, else_body))
       parse_init
       (parse_expr <* ws_line)
       (parse_block <* ws_line)
       parse_else
;;

(* можно парсить [for range 1000] как [for i := 0; i < 1000; i++] *)
let parse_for = return () (* TODO *)
let parse_range = return () (* TODO *)
let parse_break = string "break" *> return Stmt_break
let parse_continue = string "continue" *> return Stmt_continue

let parse_return =
  string "return" *> ws_line *> parse_expr >>| fun expr -> Stmt_return (Some expr)
;;

(* TODO: return multiple expressions *)

let parse_chan_send = return () (* TODO *)
let parse_chan_receive = return () (* TODO *)
let parse_stmt_call = return () (* TODO *)
let parse_defer = string "defer" *> ws_line *> return () (* заглушка *)
let parse_go = string "go" *> ws_line *> return () (* заглушка *)

let parse_stmt =
  choice
    [ parse_var_decl_any
    ; parse_assign
    ; parse_incr *> return () (* заглушка *)
    ; parse_decr *> return () (* заглушка *)
    ; parse_if *> return () (* заглушка *)
    ; parse_for
    ; parse_range
    ; parse_break *> return () (* заглушка *)
    ; parse_continue *> return () (* заглушка *)
    ; parse_return *> return ()
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
  let parse_stmts = many_sep ~sep:parse_stmt_sep ~parser:parse_stmt in
  char '{' *> ws *> parse_stmts <* ws <* char '}'
;;
