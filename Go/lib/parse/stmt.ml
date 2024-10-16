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

let parse_lvalues = sep_by1 (ws_line *> char ',' *> ws_line) parse_ident
let parse_rvalues = sep_by (ws_line *> char ',' *> ws) parse_expr

let parse_long_var_decl =
  let* _ = string "var" *> ws in
  let* lvalues = parse_lvalues in
  let* vars_type =
    ws_line *> parse_type <* ws_line >>| (fun t -> Some t) <|> ws_line *> return None
  in
  let* _ = char '=' *> ws in
  let* rvalues = parse_rvalues in
  match rvalues, List.length lvalues = List.length rvalues with
  | _ :: _, true ->
    return (Long_decl_mult_init (vars_type, combine_lists lvalues rvalues))
  | _ :: _, false ->
    if List.length rvalues = 1
    then (
      match List.nth rvalues 0 with
      | Some expr -> return (Long_decl_one_init (vars_type, lvalues, expr))
      | None -> assert false)
    else
      fail
        "Number of lvalues and rvalues in variable declarations should be the same or \
         rvalue should be a function that returns multiple values"
  | [], _ ->
    (match vars_type with
     | Some t -> return (Long_decl_no_init (t, lvalues))
     | None -> fail "Long variable declaration without initializers should have type")
;;

let parse_short_var_decl =
  let* lvalues = parse_lvalues in
  let* _ = ws_line *> string ":=" *> ws in
  let* rvalues = parse_rvalues in
  if List.length lvalues != List.length rvalues
  then
    if List.length rvalues = 1
    then (
      match List.nth rvalues 0 with
      | Some expr -> return (Stmt_short_var_decl (Short_decl_one_init (lvalues, expr)))
      | None -> assert false)
    else
      fail
        "Number of lvalues and rvalues should be the same or rvalue should be a function \
         that returns multiple values"
  else return (Stmt_short_var_decl (Short_decl_mult_init (combine_lists lvalues rvalues)))
;;

(* let parse_assign = return () *)

let parse_incr = parse_ident <* ws_line <* string "++" >>| fun id -> Stmt_incr id
let parse_decr = parse_ident <* ws_line <* string "--" >>| fun id -> Stmt_decr id

let rec parse_if pstmt pblock =
  let* _ = string "if" in
  let* init = pstmt <* parse_stmt_sep >>| (fun init -> Some init) <|> return None in
  let* cond = parse_expr <* ws_line in
  let* if_body = pblock <* ws_line in
  let* else_body =
    string "else" *> ws *> (parse_if pstmt pblock >>| fun if_stmt -> Some if_stmt)
    <|> (pblock >>| fun block -> Some (Stmt_block block))
    <|> return None
  in
  return (Stmt_if { init; cond; if_body; else_body })
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
      [ (parse_long_var_decl >>| fun decl -> Stmt_long_var_decl decl)
      ; parse_short_var_decl
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
    char '{' *> ws *> sep_by parse_stmt_sep (parse_stmt pblock) <* ws <* char '}')
;;

(**************************************** Tests ****************************************)
