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
    | None -> write_global_ident "Global Space" ident
    | Some _ ->
      fail
        (TypeCheckError
           (Multiple_declaration
              (Printf.sprintf "%s is redeclared in %s" ident "Global Space")))
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

  let retrieve_idents args = List.map (fun (x, _) -> x) args
  let check_func ident args = iter (save_local_ident ident) args

  let check_top_decl decl =
    match decl with
    | Decl_func (x, y) -> save_global_ident x *> check_func x (retrieve_idents y.args)
  ;;

  (*| Decl_var x -> None*)

  let type_check code = run (iter check_top_decl code) (MapIdent.empty, MapIdent.empty)
end

let pp ast =
  match TypeCheckMonad.type_check ast with
  | _, Result.Ok _ -> print_endline "TEST PASSED"
  | _, Result.Error x ->
    prerr_string "ERROR WHILE TYPECHECK WITH ";
    (match x with
     | TypeCheckError Check_failed -> prerr_endline "Check failed"
     | TypeCheckError (Multiple_declaration x) ->
       prerr_string "Multiple declaration error: ";
       prerr_string x
     | TypeCheckError (Undefined_ident x) ->
       prerr_endline "ERROR";
       prerr_string x)
;;

let%expect_test "multiple func declaration" =
  pp
    [ Decl_func
        ( "main"
        , { args = [ "a2", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
    ; Decl_func
        ( "main"
        , { args = [ "a1", Type_int; "a", Type_int; "b", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Multiple declaration error: main is redeclared in Global Space |}]
;;

let%expect_test "multiple declaration via args" =
  pp
    [ Decl_func
        ( "main"
        , { args = [ "a2", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
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

let%expect_test "correct declarations" =
  pp
    [ Decl_func
        ( "main"
        , { args = [ "a2", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
    ; Decl_func
        ( "foo"
        , { args = [ "a", Type_int; "b", Type_int; "c", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
    ];
  [%expect {|
    TEST PASSED |}]
;;
