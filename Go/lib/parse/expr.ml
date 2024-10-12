(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Angstrom
open Common

let parse_expr =
  choice
    [ (let* const = parse_const in
       return (Expr_const const))
    ; (let* id = parse_ident in
       return (Expr_ident id))
    ]
;;

let parse_expr_bin_op = choice []
