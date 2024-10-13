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
  pp pp_expr parse_expr "fac(4 + fac(4 + 4))";
  [%expect
    {|
    (Expr_call
       ((Expr_ident "fac"),
        [(Expr_bin_oper (Bin_sum, (Expr_const (Const_int 4)),
            (Expr_call
               ((Expr_ident "fac"),
                [(Expr_bin_oper (Bin_sum, (Expr_const (Const_int 4)),
                    (Expr_const (Const_int 4))))
                  ]))
            ))
          ]))|}]
;;

let%expect_test "fac_piece1 test" =
  pp pp_expr parse_expr "n * fac(n-1)";
  [%expect
    {|
    (Expr_bin_oper (Bin_multiply, (Expr_ident "n"),
       (Expr_call
          ((Expr_ident "fac"),
           [(Expr_bin_oper (Bin_subtract, (Expr_ident "n"),
               (Expr_const (Const_int 1))))
             ]))
       ))|}]
;;


let%expect_test "fac_piece2 test" =
  pp pp_expr parse_expr "n <= 1";
  [%expect
    {|
    (Expr_bin_oper (Bin_less_equal, (Expr_ident "n"), (Expr_const (Const_int 1))
       ))|}]
;;

let%expect_test "unary_min test" =
  pp pp_expr parse_expr "-n + 2 + -1";
  [%expect
    {|
    (Expr_bin_oper (Bin_sum,
       (Expr_bin_oper (Bin_sum, (Expr_un_oper (Unary_minus, (Expr_ident "n"))),
          (Expr_const (Const_int 2)))),
       (Expr_un_oper (Unary_minus, (Expr_const (Const_int 1))))))|}]
;;