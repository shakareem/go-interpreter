open TypeCheckMonad
open TypeCheckErrors

module TypeCheckMonad = struct
  open TypeCheckErrors
  open Ast
  include BaseMonad

  type 'a t = (type_check, 'a) BaseMonad.t

  let return_with_fail = function
    | Some x -> return x
    | None -> fail (TypeCheckError Check_failed)
  ;;

  let read_local : 'a MapIdent.t t =
    read
    >>= function
    | _, local -> return local
  ;;

  let read_local_ident ident =
    read_local >>= fun local -> MapIdent.find_opt ident local |> return
  ;;

  let read_global : 'a MapIdent.t t =
    read
    >>= function
    | global, _ -> return global
  ;;

  let read_global_ident ident =
    read_global >>= fun global -> MapIdent.find_opt ident global |> return
  ;;

  let write_local new_local =
    read
    >>= function
    | global, _ -> write (global, new_local)
  ;;

  let write_local_ident el_env el_ident =
    read_local >>= fun local -> write_local (MapIdent.add el_ident el_env local)
  ;;

  let write_global new_global =
    read
    >>= function
    | _, local -> write (new_global, local)
  ;;

  let write_global_ident el_env el_ident =
    read_global >>= fun global -> write_global (MapIdent.add el_ident el_env global)
  ;;

  let save_local_ident env ident =
    read_local_ident ident
    >>= function
    | None -> write_local_ident env ident
    | Some _ ->
      fail
        (TypeCheckError
           (Multiple_declaration (Printf.sprintf "%s is redeclared in %s" ident env)))
  ;;

  let save_global_ident ident =
    read_global_ident ident
    >>= function
    | None -> write_global_ident "Global Scope" ident
    | Some _ ->
      fail
        (TypeCheckError
           (Multiple_declaration
              (Printf.sprintf "%s is redeclared in %s" ident "Global Scope")))
  ;;

  (*
     let find_var_decl_by_name ident func_list =
     let func_name = function
     | Decl_var (x, _) -> ""
     | Decl_func _ -> ""
     in
     Stdlib.List.find_opt (fun func -> String.equal (func_name func) ident) func_list
     ;;
  *)

  let find_func name code =
    let func_name = function
      | Decl_func (func_name, _) -> func_name
      | Decl_var _ -> ""
    in
    Stdlib.List.find_opt (fun func -> String.equal (func_name func) name) code
  ;;

  let check_main code =
    let tc = function
      | Some (Decl_func (_, x)) ->
        if List.length x.args > 0
        then
          fail
            (TypeCheckError
               (Incorrect_main
                  (Printf.sprintf "func main must have no arguments and no return values")))
        else (
          match x.returns with
          | Some _ ->
            fail
              (TypeCheckError
                 (Incorrect_main
                    (Printf.sprintf
                       "func main must have no arguments and no return values")))
          | None -> return ())
      | _ -> fail (TypeCheckError (Incorrect_main (Printf.sprintf "main not found")))
    in
    tc (find_func "main" code)
  ;;

  let retrieve_paris_first args = List.map (fun (x, _) -> x) args
  let retrieve_paris_second args = List.map (fun (_, x) -> x) args

  let retrieve_idents_from_long_var_decl decl =
    match decl with
    | Long_decl_no_init (_, x, y) -> x :: y
    | Long_decl_mult_init (_, (x, _), y) -> x :: retrieve_paris_first y
    | Long_decl_one_init (_, x, y, z, _) -> y :: x :: z
  ;;

  let retrieve_idents_from_short_var_decl decl =
    match decl with
    | Short_decl_mult_init ((x, _), y) -> x :: retrieve_paris_first y
    | Short_decl_one_init (x, y, z, _) -> x :: y :: z
  ;;

  let seek_ident ident =
    read_global_ident ident
    >>= function
    | Some _ -> return ()
    | None ->
      read_local_ident ident
      >>= (function
       | Some _ -> return ()
       | None ->
         fail
           (TypeCheckError (Undefined_ident (Printf.sprintf "%s is not defined" ident))))
  ;;

  let rec check_expr expr =
    let check_func_call (Expr_ident x, y) = seek_ident x *> iter check_expr y in
    match expr with
    | Expr_ident x -> seek_ident x
    | Expr_bin_oper (_, x, y) -> check_expr x *> check_expr y
    | Expr_call x -> check_func_call x
    | Expr_chan_receive x -> check_expr x
    | Expr_index (x, y) -> check_expr x *> check_expr y
    | Expr_const _ -> return ()
    | Expr_un_oper (_, x) -> check_expr x
  ;;

  let check_func_call (Expr_ident x, y) = seek_ident x *> iter check_expr y

  let rec check_lvalue lv =
    match lv with
    | Lvalue_ident x -> seek_ident x
    | Lvalue_array_index (x, y) -> check_lvalue x *> check_expr y
  ;;

  let check_assign asgn =
    match asgn with
    | Assign_mult_expr ((x, y), z) ->
      iter check_lvalue (x :: retrieve_paris_first z)
      *> iter check_expr (y :: retrieve_paris_second z)
    | Assign_one_expr (x, y, z, w) ->
      check_lvalue x *> check_lvalue y *> iter check_lvalue z *> check_func_call w
  ;;

  let check_var_decl ident x ret = iter (save_local_ident ident) (ret x)

  let check_init ident init =
    match init with
    | Some x ->
      (match x with
       | Init_assign x -> check_assign x
       | Init_call x -> check_func_call x
       | Init_decl x -> check_var_decl ident x retrieve_idents_from_short_var_decl
       | Init_decr x -> seek_ident x
       | Init_incr x -> seek_ident x
       | Init_receive x -> check_expr x
       | Init_send (x, y) -> seek_ident x *> check_expr y)
    | None -> return ()
  ;;

  let rec check_stmt ident stmt =
    match stmt with
    | Stmt_long_var_decl x -> check_var_decl ident x retrieve_idents_from_long_var_decl
    | Stmt_short_var_decl x -> check_var_decl ident x retrieve_idents_from_short_var_decl
    | Stmt_incr x -> seek_ident x
    | Stmt_decr x -> seek_ident x
    | Stmt_assign x -> check_assign x
    | Stmt_call x -> check_func_call x
    | Stmt_defer x -> check_func_call x
    | Stmt_go x -> check_func_call x
    | Stmt_chan_send (x, y) -> seek_ident x *> check_expr y
    | Stmt_block x -> iter (check_stmt ident) x
    | Stmt_break -> return ()
    | Stmt_chan_receive x -> check_expr x
    | Stmt_continue -> return ()
    | Stmt_return x -> iter check_expr x
    | Stmt_if x ->
      check_init ident x.init
      *> check_expr x.cond
      *> iter (check_stmt ident) x.if_body
      *>
        (match x.else_body with
        | Some x ->
          (match x with
           | Else_block x -> iter (check_stmt ident) x
           | Else_if x -> check_stmt ident (Stmt_if x))
        | None -> return ())
    | Stmt_for x ->
      check_init ident x.init
      *> check_init ident x.post
      *> iter (check_stmt ident) x.body
      *>
        (match x.cond with
        | Some x -> check_expr x
        | None -> return ())
  ;;

  let check_func ident args body =
    iter (save_local_ident ident) args *> iter (check_stmt ident) body
  ;;

  let check_top_decl_funcs decl =
    match decl with
    | Decl_func (x, _) -> write_local MapIdent.empty *> save_global_ident x
    | Decl_var x -> iter save_global_ident (retrieve_idents_from_long_var_decl x)
  ;;

  let check_top_decl decl =
    match decl with
    | Decl_func (x, y) -> check_func x (retrieve_paris_first y.args) y.body
    | Decl_var _ -> return ()
  ;;

  let type_check code =
    run
      (check_main code *> iter check_top_decl_funcs code *> iter check_top_decl code)
      (MapIdent.empty, MapIdent.empty)
  ;;
end

let pp ast =
  match TypeCheckMonad.type_check ast with
  | _, Result.Ok _ -> print_endline "CORRECT"
  | _, Result.Error x ->
    prerr_string "ERROR WHILE TYPECHECK WITH ";
    (match x with
     | TypeCheckError Check_failed -> prerr_endline "Check failed"
     | TypeCheckError (Multiple_declaration x) ->
       prerr_string "Multiple declaration error: ";
       prerr_string x
     | TypeCheckError (Incorrect_main x) ->
       prerr_endline "Incorrect main error:";
       prerr_string x
     | TypeCheckError (Undefined_ident x) ->
       prerr_endline "Undefined ident error:";
       prerr_string x)
;;

let%expect_test "multiple func declaration" =
  pp
    [ Decl_func ("main", { args = []; returns = None; body = [] })
    ; Decl_func ("main", { args = []; returns = None; body = [] })
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Multiple declaration error: main is redeclared in Global Scope |}]
;;

let%expect_test "multiple declaration via args" =
  pp
    [ Decl_func ("main", { args = []; returns = None; body = [] })
    ; Decl_func
        ( "foo"
        , { args = [ "a", Type_int; "a", Type_int; "b", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Multiple declaration error: a is redeclared in foo |}]
;;

let%expect_test "multiple declaration in global space" =
  pp
    [ Decl_var
        (Long_decl_one_init
           ( Some (Type_chan (Chan_receive, Type_array (5, Type_int)))
           , "a"
           , "foo"
           , [ "c" ]
           , (Expr_ident "get", []) ))
    ; Decl_var (Long_decl_no_init (Type_int, "x", []))
    ; Decl_func ("main", { args = []; returns = None; body = [] })
    ; Decl_func
        ( "foo"
        , { args = [ "a1", Type_int; "c", Type_int; "b", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Multiple declaration error: foo is redeclared in Global Scope |}]
;;

let%expect_test "correct declarations #1" =
  pp
    [ Decl_func ("main", { args = []; returns = None; body = [] })
    ; Decl_func
        ( "foo"
        , { args = [ "a", Type_int; "b", Type_int; "c", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
    ; Decl_func
        ( "foo1"
        , { args = [ "a", Type_int; "b", Type_int; "c", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Multiple declaration error: a is redeclared in foo1 |}]
;;

let%expect_test "correct declarations #2" =
  pp
    [ Decl_var
        (Long_decl_one_init
           ( Some (Type_chan (Chan_receive, Type_array (5, Type_int)))
           , "a"
           , "b"
           , [ "c" ]
           , (Expr_ident "get", []) ))
    ; Decl_var (Long_decl_no_init (Type_int, "x", []))
    ; Decl_func ("main", { args = []; returns = None; body = [] })
    ; Decl_func
        ( "foo1"
        , { args = [ "a1", Type_int; "c", Type_int; "b", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
    ; Decl_func
        ( "foo2"
        , { args = [ "a1", Type_int; "c", Type_int; "b", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Multiple declaration error: a1 is redeclared in foo2 |}]
;;

let%expect_test "undefined var inc" =
  pp
    [ Decl_var (Long_decl_no_init (Type_int, "x", []))
    ; Decl_func ("main", { args = []; returns = None; body = [] })
    ; Decl_func
        ( "foo"
        , { args = [ "a1", Type_int; "c", Type_int; "b", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = [ Stmt_incr "a2" ]
          } )
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Undefined ident error:
    a2 is not defined |}]
;;

let%expect_test "undefined func call" =
  pp
    [ Decl_var (Long_decl_no_init (Type_int, "x", []))
    ; Decl_func
        ( "main"
        , { args = []; returns = None; body = [ Stmt_call (Expr_ident "foo2", []) ] } )
    ; Decl_func
        ( "foo"
        , { args = [ "a1", Type_int; "c", Type_int; "b", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Undefined ident error:
    foo2 is not defined |}]
;;

let%expect_test "multiple declarations in func body with args" =
  pp
    [ Decl_var
        (Long_decl_one_init
           ( Some (Type_chan (Chan_receive, Type_array (5, Type_int)))
           , "a"
           , "b"
           , [ "c" ]
           , (Expr_ident "get", []) ))
    ; Decl_var (Long_decl_no_init (Type_int, "x", []))
    ; Decl_func
        ( "foo"
        , { args = [ "a2", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = [ Stmt_long_var_decl (Long_decl_no_init (Type_int, "a2", [])) ]
          } )
    ; Decl_func ("main", { args = []; returns = None; body = [] })
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Multiple declaration error: a2 is redeclared in foo |}]
;;

let%expect_test "main with returns and args" =
  pp
    [ Decl_var
        (Long_decl_one_init
           ( Some (Type_chan (Chan_receive, Type_array (5, Type_int)))
           , "a"
           , "b"
           , [ "c" ]
           , (Expr_ident "get", []) ))
    ; Decl_var (Long_decl_no_init (Type_int, "x", []))
    ; Decl_func
        ( "main"
        , { args = [ "a", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
    ; Decl_func
        ( "main1"
        , { args = [ "a1", Type_int; "c", Type_int; "b", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Incorrect main error:
    func main must have no arguments and no return values |}]
;;

let%expect_test "arg not declared" =
  pp
    [ Decl_func
        ( "main"
        , { args = []
          ; returns = None
          ; body = [ Stmt_block [ Stmt_call (Expr_ident "println", [ Expr_ident "a" ]) ] ]
          } )
    ; Decl_func ("println", { args = []; returns = None; body = [] })
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Undefined ident error:
    a is not defined |}]
;;

let%expect_test "correct fac" =
  pp
    [ Decl_func
        ( "main"
        , { args = []
          ; returns = None
          ; body =
              [ Stmt_block [ Stmt_call (Expr_ident "fac", [ Expr_const (Const_int 6) ]) ]
              ]
          } )
    ; Decl_func
        ( "fac"
        , { args = [ "n", Type_int ]
          ; returns = Some (Only_types (Type_int, []))
          ; body =
              [ Stmt_if
                  { init = None
                  ; cond =
                      Expr_bin_oper (Bin_equal, Expr_ident "n", Expr_const (Const_int 1))
                  ; if_body = [ Stmt_return [ Expr_const (Const_int 1) ] ]
                  ; else_body =
                      Some
                        (Else_block
                           [ Stmt_return
                               [ Expr_bin_oper
                                   ( Bin_multiply
                                   , Expr_ident "n"
                                   , Expr_call
                                       ( Expr_ident "fac"
                                       , [ Expr_bin_oper
                                             ( Bin_subtract
                                             , Expr_ident "n"
                                             , Expr_const (Const_int 1) )
                                         ] ) )
                               ]
                           ])
                  }
              ]
          } )
    ];
  [%expect {|
    CORRECT |}]
;;

let%expect_test "unknown var in if cond" =
  pp
    [ Decl_func
        ( "main"
        , { args = []
          ; returns = None
          ; body =
              [ Stmt_block [ Stmt_call (Expr_ident "fac", [ Expr_const (Const_int 6) ]) ]
              ]
          } )
    ; Decl_func
        ( "fac"
        , { args = [ "n", Type_int ]
          ; returns = Some (Only_types (Type_int, []))
          ; body =
              [ Stmt_if
                  { init = None
                  ; cond =
                      Expr_bin_oper (Bin_equal, Expr_ident "a", Expr_const (Const_int 1))
                  ; if_body = [ Stmt_return [ Expr_const (Const_int 1) ] ]
                  ; else_body =
                      Some
                        (Else_block
                           [ Stmt_return
                               [ Expr_bin_oper
                                   ( Bin_multiply
                                   , Expr_ident "n"
                                   , Expr_call
                                       ( Expr_ident "fac"
                                       , [ Expr_bin_oper
                                             ( Bin_subtract
                                             , Expr_ident "n"
                                             , Expr_const (Const_int 1) )
                                         ] ) )
                               ]
                           ])
                  }
              ]
          } )
    ];
  [%expect {|
    ERROR WHILE TYPECHECK WITH Undefined ident error:
    a is not defined |}]
;;
