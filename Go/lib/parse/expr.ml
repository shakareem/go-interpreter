(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Angstrom
open Common

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a
let token s = ws *> string s <* ws
let pesum = token "+" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_sum, exp1, exp2))

let pemul =
  token "*" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_multiply, exp1, exp2))
;;

let pesub =
  token "-" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_subtract, exp1, exp2))
;;

let pediv = token "/" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_divide, exp1, exp2))
let pemod = token "%" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_modulus, exp1, exp2))
let peeql = token "==" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_equal, exp1, exp2))

let penql =
  token "!= " *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_not_equal, exp1, exp2))
;;

let pegrt = token ">" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_greater, exp1, exp2))

let pegre =
  token ">=" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_greater_equal, exp1, exp2))
;;

let pelss = token "<" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_less, exp1, exp2))

let pelse =
  token "<=" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_less_equal, exp1, exp2))
;;

let peand = token "&&" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_and, exp1, exp2))
let peor = token "||" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_or, exp1, exp2))

let parse_simple_expr =
  choice
    [ (let* const = parse_const in
       return (Expr_const const))
    ; (let* id = parse_ident in
       return (Expr_ident id))
    ]
;;

let parse_func_call pexpr =
  lift2
    (fun id ls -> Expr_call (Expr_ident id, ls))
    (ws *> parse_ident)
    (token "(" *> many_sep (token ",") pexpr <* token ")")
;;

let parse_expr =
  fix (fun expr ->
    let arg = choice [ parse_func_call expr; parse_simple_expr ] in
    let arg = chainl1 arg (pemul <|> pemod <|> pediv) in
    let arg = chainl1 arg (pesum <|> pesub) in
    let arg = fix (fun expr -> arg) <|> arg in
    arg)
;;

let parse_expr_bin_op = choice []
