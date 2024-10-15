(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Angstrom
open Common
open Expr

let rec combine_lists l1 l2 =
  match l1, l2 with
  | [], [] -> []
  | x :: xs, y :: ys -> (x, y) :: combine_lists xs ys
  | _, _ -> assert false
;;

let parse_inits =
  ws_line
  *> char '='
  *> ws
  *> many_sep ~sep:(ws_line *> char ',' *> ws) ~parser:parse_expr
;;

let parse_idents =
  string "var" *> ws *> many_sep ~sep:(ws_line *> char ',' *> ws_line) ~parser:parse_ident
;;

(* Decl_with_init (None, []) should cause error, will be proccessed at interpretation state *)
let parse_var_decl_top_level =
  let* idents = parse_idents in
  let* vars_type =
    ws_line *> parse_type <* ws_line >>| (fun t -> Some t) <|> ws_line *> return None
  in
  let* _ = char '=' *> ws in
  let* inits = parse_inits in
  let build_var_decl_parser idents vars_type inits =
    match vars_type, inits with
    | Some t, _ :: _ ->
      if List.length idents != List.length inits
      then Decl_with_init (None, [])
      else Decl_with_init (Some t, combine_lists idents inits)
    | Some t, [] -> Decl_no_init (t, idents)
    | None, _ :: _ ->
      if List.length idents != List.length inits
      then Decl_with_init (None, [])
      else Decl_with_init (Some Type_int, combine_lists idents inits)
    | None, [] -> Decl_with_init (None, [])
  in
  return (build_var_decl_parser idents vars_type inits)
;;

(* let parse_var_decl_in_func = return () *)
let parse_var_decl_any =
  parse_var_decl_top_level
  (* <|> parse_var_decl_in_func *) >>| fun decl -> Stmt_var_decl decl
;;

(* let parse_assign = return () *)

let parse_incr = parse_ident <* ws_line <* string "++" >>| fun id -> Stmt_incr id
let parse_decr = parse_ident <* ws_line <* string "--" >>| fun id -> Stmt_decr id

(* в ините нужно парсить только parse_var_decl_in_func *)
let parse_if pstmt pblock =
  let* _ = string "if" in
  let* init = pstmt <* parse_stmt_sep >>| (fun init -> Some init) <|> return None in
  let* cond = parse_expr <* ws_line in
  let* if_body = pblock <* ws_line in
  let* else_body =
    string "else" *> ws *> pblock >>| (fun block -> Some block) <|> return None
  in
  (fun init cond if_body else_body -> return (Stmt_if (init, cond, if_body, else_body)))
    init
    cond
    if_body
    else_body
;;

(* можно парсить [for range 1000] как [for i := 0; i < 1000; i++]
   let parse_for = return ()
   let parse_range = return () *)
let parse_break = string "break" *> return Stmt_break
let parse_continue = string "continue" *> return Stmt_continue

(* TODO: return multiple expressions *)
let parse_return =
  string "return" *> ws_line *> parse_expr >>| fun expr -> Stmt_return (Some expr)
;;

(* let parse_chan_send = return ()
   let parse_chan_receive = return ()
   let parse_stmt_call = return ()
   let parse_defer = string "defer" *> ws_line *> return ()
   let parse_go = string "go" *> ws_line *> return () *)

let parse_stmt pblock =
  fix (fun pstmt ->
    choice
      [ parse_var_decl_any
      ; parse_incr
      ; parse_decr
      ; parse_if pstmt pblock
      ; parse_break
      ; parse_continue
      ; parse_return
        (*  ; parse_assign
            ; parse_for
            ; parse_range
            ; parse_chan_send
            ; parse_chan_receive
            ; parse_stmt_call
            ; parse_defer
            ; parse_go *)
      ]
      ~failure_msg:"Incorrect statement")
;;

let parse_block : block t =
  fix (fun pblock ->
    let parse_stmts = many_sep ~sep:parse_stmt_sep ~parser:(parse_stmt pblock) in
    char '{' *> ws *> parse_stmts <* ws <* char '}')
;;
