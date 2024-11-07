(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Pp
open Expr
open Stmt

let pexpr = parse_expr parse_block

(********** const int and string **********)

let%expect_test "const int" =
  pp pp_expr pexpr {|256|};
  [%expect {| (Expr_const (Const_int 256)) |}]
;;

let%expect_test "zero" =
  pp pp_expr pexpr {|0|};
  [%expect {| (Expr_const (Const_int 0)) |}]
;;

let%expect_test "not digit in int" =
  pp pp_expr pexpr {|123,321|};
  [%expect {| : end_of_input |}]
;;

(* bug
let%expect_test "very big int" =
  pp pp_expr pexpr {|9999999999999999999999999999999999999999|};
  [%expect {||}]
;; *)

let%expect_test "const string" =
  pp pp_expr pexpr {|"my_string"|};
  [%expect {| (Expr_const (Const_string "my_string")) |}]
;;

let%expect_test "string with '\n'" =
  pp pp_expr pexpr {|"Hello\n"|};
  [%expect {| (Expr_const (Const_string "Hello\\n")) |}]
;;

(********** const array **********)

let%expect_test "expr simple array" =
  pp pp_expr pexpr {|[3]int{}|};
  [%expect
    {|
    (Expr_const
       (Const_array (Type_int,
          [(Expr_const (Const_int 0)); (Expr_const (Const_int 0));
            (Expr_const (Const_int 0))]
          ))) |}]
;;

let%expect_test "expr array with init" =
  pp pp_expr pexpr {|[3]int{1, 2}|};
  [%expect
    {|
    (Expr_const
       (Const_array (Type_int,
          [(Expr_const (Const_int 1)); (Expr_const (Const_int 2));
            (Expr_const (Const_int 0))]
          ))) |}]
;;

(********** ident **********)

let%expect_test "expr ident false" =
  pp pp_expr pexpr {|false|};
  [%expect {|
    (Expr_ident "false")|}]
;;

let%expect_test "expr ident nil" =
  pp pp_expr pexpr {|nil|};
  [%expect {|
    (Expr_ident "nil")|}]
;;

let%expect_test "expr ident" =
  pp pp_expr pexpr {|abcdefg__|};
  [%expect {|
    (Expr_ident "abcdefg__")|}]
;;

let%expect_test "expr ident in braces" =
  pp pp_expr pexpr {|(abc)|};
  [%expect {|
    (Expr_ident "abc")|}]
;;

(********** func call **********)

let%expect_test "simple func call" =
  pp pp_expr pexpr "a()";
  [%expect {|
    (Expr_call ((Expr_ident "a"), []))|}]
;;

let%expect_test "func call with multiple complex arguments" =
  pp pp_expr pexpr "three(abc, 2 + 3, fac(25))";
  [%expect
    {|
    (Expr_call
       ((Expr_ident "three"),
        [(Expr_ident "abc");
          (Expr_bin_oper (Bin_sum, (Expr_const (Const_int 2)),
             (Expr_const (Const_int 3))));
          (Expr_call ((Expr_ident "fac"), [(Expr_const (Const_int 25))]))]))|}]
;;

let%expect_test "nested func call" =
  pp pp_expr pexpr "a()()()";
  [%expect
    {|
    (Expr_call ((Expr_call ((Expr_call ((Expr_ident "a"), [])), [])), []))|}]
;;

(********** index **********)

let%expect_test "index with idents" =
  pp pp_expr pexpr {|array[i]|};
  [%expect {| (Expr_index ((Expr_ident "array"), (Expr_ident "i"))) |}]
;;

let%expect_test "index with constant array" =
  pp pp_expr pexpr {|[3]int{1, 2, 3}[0]|};
  [%expect
    {|
    (Expr_index (
       (Expr_const
          (Const_array (Type_int,
             [(Expr_const (Const_int 1)); (Expr_const (Const_int 2));
               (Expr_const (Const_int 3))]
             ))),
       (Expr_const (Const_int 0)))) |}]
;;

let%expect_test "index with function call in index" =
  pp pp_expr pexpr {|array[get_index(a, b)]|};
  [%expect
    {|
    (Expr_index ((Expr_ident "array"),
       (Expr_call
          ((Expr_ident "get_index"), [(Expr_ident "a"); (Expr_ident "b")]))
       )) |}]
;;

let%expect_test "index with function call as an array" =
  pp pp_expr pexpr {|get_array(a, b)[1]|};
  [%expect
    {|
    (Expr_index (
       (Expr_call
          ((Expr_ident "get_array"), [(Expr_ident "a"); (Expr_ident "b")])),
       (Expr_const (Const_int 1)))) |}]
;;

let%expect_test "nested indicies" =
  pp pp_expr pexpr {|a[1][2][3]|};
  [%expect
    {|
    (Expr_index (
       (Expr_index ((Expr_index ((Expr_ident "a"), (Expr_const (Const_int 1)))),
          (Expr_const (Const_int 2)))),
       (Expr_const (Const_int 3)))) |}]
;;

(********** complex exprs **********)

let%expect_test "expr logical operations" =
  pp pp_expr pexpr {|a && (b || c)|};
  [%expect
    {|
    (Expr_bin_oper (Bin_and, (Expr_ident "a"),
       (Expr_bin_oper (Bin_or, (Expr_ident "b"), (Expr_ident "c")))))|}]
;;

let%expect_test "expr bin mult and sum" =
  pp pp_expr pexpr {|-5 * _r + 8|};
  [%expect
    {|
    (Expr_bin_oper (Bin_sum,
       (Expr_bin_oper (Bin_multiply,
          (Expr_un_oper (Unary_minus, (Expr_const (Const_int 5)))),
          (Expr_ident "_r"))),
       (Expr_const (Const_int 8))))|}]
;;

let%expect_test "expr_call test" =
  pp pp_expr pexpr "fac(4 + fac(4 + 4))";
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
  pp pp_expr pexpr "n * fac(n-1)";
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
  pp pp_expr pexpr "n <= 1";
  [%expect
    {|
    (Expr_bin_oper (Bin_less_equal, (Expr_ident "n"), (Expr_const (Const_int 1))
       ))|}]
;;

let%expect_test "unary_min test" =
  pp pp_expr pexpr "-n + 2 + -1";
  [%expect
    {|
    (Expr_bin_oper (Bin_sum,
       (Expr_bin_oper (Bin_sum, (Expr_un_oper (Unary_minus, (Expr_ident "n"))),
          (Expr_const (Const_int 2)))),
       (Expr_un_oper (Unary_minus, (Expr_const (Const_int 1))))))|}]
;;

let%expect_test "channel recieve test" =
  pp pp_expr pexpr "<-c + 1";
  [%expect
    {|
    (Expr_bin_oper (Bin_sum, (Expr_chan_recieve "c"), (Expr_const (Const_int 1))
       ))|}]
;;

(********** anon func **********)

let%expect_test "empty anon func" =
  pp pp_expr pexpr {|func() {}|};
  [%expect {| (Expr_const (Const_func { args = []; returns = None; body = [] })) |}]
;;

let%expect_test "anon func with one arg and one return value" =
  pp pp_expr pexpr {|func(a int) int { return a }|};
  [%expect
    {|
    (Expr_const
       (Const_func
          { args = [("a", Type_int)]; returns = (Some (Only_types [Type_int]));
            body = [(Stmt_return [(Expr_ident "a")])] })) |}]
;;

let%expect_test "anon func with mult args and return values" =
  pp pp_expr pexpr {|func(a int, b string) (int, string) { return a, b }|};
  [%expect
    {|
    (Expr_const
       (Const_func
          { args = [("a", Type_int); ("b", Type_string)];
            returns = (Some (Only_types [Type_int; Type_string]));
            body = [(Stmt_return [(Expr_ident "a"); (Expr_ident "b")])] })) |}]
;;

let%expect_test "anon func with mult args and named return values" =
  pp
    pp_expr
    pexpr
    {|func(a int, b string) (res1 int, res2 string) { res1, res2 = a, b; return }|};
  [%expect
    {|
    (Expr_const
       (Const_func
          { args = [("a", Type_int); ("b", Type_string)];
            returns =
            (Some (Ident_and_types [("res1", Type_int); ("res2", Type_string)]));
            body =
            [(Stmt_assign
                (Assign_mult_expr
                   [((Lvalue_ident "res1"), (Expr_ident "a"));
                     ((Lvalue_ident "res2"), (Expr_ident "b"))]));
              (Stmt_return [])]
            })) |}]
;;
