(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Angstrom
open Common
open Expr
open Stmt

let parse_func_decl : func_decl t =
  let* _ = string "func" *> ws in
  let* func_name = parse_ident <* ws_line in
  let* args_returns_and_body = parse_func_args_returns_and_body parse_block in
  return (func_name, args_returns_and_body)
;;

let parse_top_decl =
  parse_long_var_decl
  >>| (fun decl -> Decl_var decl)
  <|> (parse_func_decl >>| fun decl -> Decl_func decl)
;;

let parse_file : file t = ws *> sep_by parse_stmt_sep parse_top_decl <* ws

(**************************************** Tests ****************************************)

let%expect_test "file with one var decl with ws" =
  pp pp_file parse_file {|

    /* hello */  var a int
// hey

|};
  [%expect {|
    [(Decl_var (Long_decl_no_init (Type_int, ["a"])))] |}]
;;

let%expect_test "file with multiple var decls separated by semicolon" =
  pp pp_file parse_file {|var a, b int;var c = "hello"|};
  [%expect
    {|
    [(Decl_var (Long_decl_no_init (Type_int, ["a"; "b"])));
      (Decl_var
         (Long_decl_mult_init (None, [("c", (Expr_const (Const_string "hello")))]
            )))
      ] |}]
;;

let%expect_test "file with one simple func decl" =
  pp pp_file parse_file {|func _() {}|};
  [%expect {|
    [(Decl_func ("_", { args = []; returns = None; body = [] }))] |}]
;;

let%expect_test "file with one default func decl" =
  pp pp_file parse_file {|func sum3(a, b, c int) int {
        return a + b + c
  }|};
  [%expect
    {|
    [(Decl_func
        ("sum3",
         { args = [("a", Type_int); ("b", Type_int); ("c", Type_int)];
           returns = (Some (Only_types [Type_int]));
           body =
           [(Stmt_return
               [(Expr_bin_oper (Bin_sum,
                   (Expr_bin_oper (Bin_sum, (Expr_ident "a"), (Expr_ident "b"))),
                   (Expr_ident "c")))
                 ])
             ]
           }))
      ] |}]
;;

let%expect_test "file with one complex func decl" =
  pp
    pp_file
    parse_file
    {|func test(a, b int, c string) (sum int, c string) {
        sum = a + b
        s = c
        return
  }|};
  [%expect
    {|
    [(Decl_func
        ("test",
         { args = [("a", Type_int); ("b", Type_int); ("c", Type_string)];
           returns =
           (Some (Ident_and_types [("sum", Type_int); ("c", Type_string)]));
           body =
           [(Stmt_assign
               (Assign_mult_expr
                  [("sum",
                    (Expr_bin_oper (Bin_sum, (Expr_ident "a"), (Expr_ident "b"))))
                    ]));
             (Stmt_assign (Assign_mult_expr [("s", (Expr_ident "c"))]));
             (Stmt_return [])]
           }))
      ] |}]
;;

let%expect_test "file with mixed func and var decls" =
  pp
    pp_file
    parse_file
    {|

var a = 5

func test (
// hey
) (

/* hello */	) { 
	return 
}

func id(a int) (int) {
	return a
}

var f int

func main() {
	defer test()

go println(id(10))
}

|};
  [%expect
    {|
    [(Decl_var (Long_decl_mult_init (None, [("a", (Expr_const (Const_int 5)))])));
      (Decl_func
         ("test", { args = []; returns = None; body = [(Stmt_return [])] }));
      (Decl_func
         ("id",
          { args = [("a", Type_int)]; returns = (Some (Only_types [Type_int]));
            body = [(Stmt_return [(Expr_ident "a")])] }));
      (Decl_var (Long_decl_no_init (Type_int, ["f"])));
      (Decl_func
         ("main",
          { args = []; returns = None;
            body =
            [(Stmt_defer ((Expr_ident "test"), []));
              (Stmt_go
                 ((Expr_ident "println"),
                  [(Expr_call ((Expr_ident "id"), [(Expr_const (Const_int 10))]))
                    ]))
              ]
            }))
      ] |}]
;;
