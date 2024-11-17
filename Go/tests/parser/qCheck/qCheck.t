Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev
SPDX-License-Identifier: MIT

  $ ./qCheckRun.exe --seed 291010702
   seed: 291010702
  
  --- Failure --------------------------------------------------------------------
  
  Test QCheck test failed (54 shrink steps):
  
  [(Decl_var
      (Long_decl_mult_init (None,
         ("a",
          (Expr_call
             ((Expr_ident "a"),
              [(Expr_call
                  ((Expr_ident "a"),
                   [(Expr_index (
                       (Expr_const
                          (Const_func
                             { args = []; returns = None;
                               body =
                               [(Stmt_assign
                                   (Assign_mult_expr (
                                      ((Lvalue_ident "a"),
                                       (Expr_const
                                          (Const_array (0, Type_bool,
                                             [(Expr_index ((Expr_ident "a"),
                                                 (Expr_call
                                                    ((Expr_call
                                                        ((Expr_ident "a"),
                                                         [(Expr_const
                                                             (Const_func
                                                                { args = [];
                                                                  returns =
                                                                  None;
                                                                  body =
                                                                  [(Stmt_return
                                                                      [(Expr_index (
                                                                      (Expr_ident
                                                                      "a"),
                                                                      (Expr_index (
                                                                      (Expr_call
                                                                      ((
                                                                      Expr_call
                                                                      ((
                                                                      Expr_const
                                                                      (Const_func
                                                                      { args =
                                                                      [];
                                                                      returns =
                                                                      None;
                                                                      body =
                                                                      [(Stmt_assign
                                                                      (Assign_mult_expr (
                                                                      ((
                                                                      Lvalue_ident
                                                                      "a"),
                                                                      (Expr_call
                                                                      ((
                                                                      Expr_ident
                                                                      "a"),
                                                                      [(Expr_const
                                                                      (Const_func
                                                                      { args =
                                                                      [];
                                                                      returns =
                                                                      None;
                                                                      body =
                                                                      [(Stmt_short_var_decl
                                                                      (Short_decl_mult_init (
                                                                      ("a",
                                                                      (Expr_call
                                                                      ((
                                                                      Expr_ident
                                                                      "a"),
                                                                      [(Expr_const
                                                                      (Const_array (
                                                                      0,
                                                                      Type_string,
                                                                      [(Expr_const
                                                                      (Const_array (
                                                                      0,
                                                                      Type_int,
                                                                      [(Expr_call
                                                                      ((
                                                                      Expr_ident
                                                                      "a"),
                                                                      [(Expr_index (
                                                                      (Expr_ident
                                                                      "a"),
                                                                      (Expr_ident
                                                                      "go")))]))
                                                                      ])))])))]))),
                                                                      [])))] }))
                                                                      ]))), 
                                                                      [])))] })),
                                                                      [])), 
                                                                      [])),
                                                                      (Expr_ident
                                                                      "a")))))])
                                                                    ]
                                                                  }))
                                                           ])),
                                                     []))
                                                 ))
                                               ]
                                             )))),
                                      [])))
                                 ]
                               })),
                       (Expr_ident "a")))
                     ]))
                ]))),
         [])))
    ]
  ================================================================================
  failure (1 tests failed, 0 tests errored, ran 1 tests)
  [1]
