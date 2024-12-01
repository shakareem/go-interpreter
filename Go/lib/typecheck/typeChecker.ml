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
    | None -> write_global_ident "_global space" ident
    | Some _ ->
      fail
        (TypeCheckError
           (Multiple_declaration
              (Printf.sprintf "%s is redeclared in %s" ident "_global space")))
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

  let retrieve_idents_from_pair args = List.map (fun (x, _) -> x) args

  let retrieve_idents_from_long_var_decl decl =
    match decl with
    | Long_decl_no_init (_, x, y) -> x :: y
    | Long_decl_mult_init (_, (x, _), y) -> x :: retrieve_idents_from_pair y
    | Long_decl_one_init (_, x, y, z, _) -> y :: x :: z
  ;;

  let retrieve_idents_from_short_var_decl decl =
    match decl with
    | Short_decl_mult_init ((x, _), y) -> x :: retrieve_idents_from_pair y
    | Short_decl_one_init (x, y, z, _) -> x :: y :: z
  ;;

  let check_afunc ident stmt =
    match stmt with
    | Stmt_long_var_decl x ->
      iter (save_local_ident ident) (retrieve_idents_from_long_var_decl x)
    | Stmt_short_var_decl x ->
      iter (save_local_ident ident) (retrieve_idents_from_short_var_decl x)
    | _ -> return ()
  ;;

  let check_func ident args body =
    iter (save_local_ident ident) args *> iter (check_afunc ident) body
  ;;

  let check_top_decl decl =
    match decl with
    | Decl_func (x, y) ->
      write_local MapIdent.empty
      *> save_global_ident x
      *> check_func x (retrieve_idents_from_pair y.args) y.body
    | Decl_var x -> iter save_global_ident (retrieve_idents_from_long_var_decl x)
  ;;

  let type_check code =
    run (check_main code *> iter check_top_decl code) (MapIdent.empty, MapIdent.empty)
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
       prerr_endline "ERROR";
       prerr_string x)
;;

let%expect_test "multiple func declaration" =
  pp
    [ Decl_func ("main", { args = []; returns = None; body = [] })
    ; Decl_func ("main", { args = []; returns = None; body = [] })
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Multiple declaration error: main is redeclared in _global space |}]
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
    ERROR WHILE TYPECHECK WITH Multiple declaration error: foo is redeclared in _global space |}]
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
  [%expect {|
    CORRECT |}]
;;

let%expect_test "correct declaration s #2" =
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
  [%expect {|
    CORRECT |}]
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
    ERROR WHILE TYPECHECK WITH ERROR
    func main must have no arguments and no return values |}]
;;
