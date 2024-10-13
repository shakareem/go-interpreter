(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Expr
open Pp

(*    {|
    (Expr_bin_oper (Bin_substract, (Expr_call (Expr_ident "factorial", Const_int 5)),
     (Expr_call (Expr_ident "factorial", Const_int 4))))|}]
*)
let%expect_test "expr_call test" =
  pp pp_expr parse_expr "fac(4) + fac(5)";
  [%expect
    {|
    (Expr_bin_oper (Bin_sum,
       (Expr_call ((Expr_ident "fac"), [(Expr_const (Const_int 4))])),
       (Expr_call ((Expr_ident "fac"), [(Expr_const (Const_int 5))]))))|}]
;;
