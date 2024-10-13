(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Expr
open Pp

let%expect_test "expr_call test" =
  pp pp_expr parse_expr "fac(5) - fac(4)";
  [%expect
    {|
    Error: Wrong Syntax|}]
;;
