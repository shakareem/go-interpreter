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
  | _, _ -> assert false (* bad, mb [] instead *)
;;

let parse_lvalues =
  many_sep ~sep:(ws_line *> char ',' *> ws_line) ~parser:parse_decl_ident
;;

let parse_rvalues = many_sep ~sep:(ws_line *> char ',' *> ws) ~parser:parse_expr

let parse_long_var_decl =
  let* _ = string "var" *> ws in
  let* lvalues = parse_lvalues in
  let* vars_type =
    ws_line *> parse_type <* ws_line >>| (fun t -> Some t) <|> ws_line *> return None
  in
  let* _ = char '=' *> ws in
  let* rvalues = parse_rvalues in
  let build_var_decl_parser lvalues vars_type rvalues =
    match vars_type, rvalues with
    | Some t, _ :: _ ->
      if List.length lvalues != List.length rvalues
      then Decl_with_init (None, [])
      else Decl_with_init (Some t, combine_lists lvalues rvalues)
    | Some t, [] -> Decl_no_init (t, lvalues)
    | None, _ :: _ ->
      if List.length lvalues != List.length rvalues
      then Decl_with_init (None, [])
      else Decl_with_init (None, combine_lists lvalues rvalues)
    | None, [] -> Decl_with_init (None, [])
  in
  match build_var_decl_parser lvalues vars_type rvalues with
  | Decl_with_init (None, []) ->
    fail
      "Var declaration has to have either type or rvalues and number of lvalues and \
       rvalues should be the same"
  | result -> return result
;;

let parse_short_var_decl =
  let* lvalues = parse_lvalues in
  let* _ = ws_line *> string ":=" *> ws in
  let* rvalues = parse_rvalues in
  if List.length lvalues != List.length rvalues
  then fail "Number of lvalues and rvalues should be the same"
  else return (Decl_with_init (None, combine_lists lvalues rvalues))
;;

let parse_var_decl_any =
  parse_long_var_decl <|> parse_short_var_decl >>| fun decl -> Stmt_var_decl decl
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
  return (Stmt_if (init, cond, if_body, else_body))
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
