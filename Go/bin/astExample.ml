(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast

let () =
  let factorial_ast : func_decl =
    { func_name = "factorial" (* function identificator *)
    ; args = Some [ "n", Some Type_int ] (* arguments *)
    ; return_types =
        (* return types *)
        Some [ (* variable name *) None, (* type *) Some Type_int ]
    ; body =
        (* function body *)
        Stmt_block
          [ Stmt_if
              (* if statement *)
              ( None (* initialization *)
              , (* condition *)
                Expr_bin_oper (Bin_equal, Expr_ident "n", Expr_const (Const_int 0))
              , (* "if" branch *)
                Stmt_return (Some (Expr_const (Const_int 1)))
              , (* "else" branch *)
                Some
                  (Stmt_return
                     (Some
                        (Expr_bin_oper
                           ( Bin_multiply
                           , Expr_ident "n"
                           , Expr_call
                               ( Expr_ident "factorial"
                               , Some
                                   [ Expr_bin_oper
                                       ( Bin_subtract
                                       , Expr_ident "n"
                                       , Expr_const (Const_int 1) )
                                   ] ) )))) )
          ]
    }
  in
  print_endline (show_func_decl factorial_ast)
;;