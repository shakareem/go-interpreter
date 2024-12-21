(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open TypeCheckMonad
open TypeCheckMonad.CheckMonad
open Errors
open Ast

let lpf args = List.map (fun (fst, _) -> fst) args
let lps args = List.map (fun (_, snd) -> snd) args

let get_anon_func_type anon_func =
  let args = lps anon_func.args in
  match anon_func.returns with
  | Some (hd, tl) -> Ctype (Type_func (args, hd :: tl))
  | None -> Ctype (Type_func (args, []))
;;

let check_anon_func anon_func cstmt =
  let save_args = iter (fun (id, t) -> save_local_ident id (Ctype t)) anon_func.args in
  write_env
  *> (match anon_func.returns with
    | Some (t, []) -> write_func (Ctuple [ t ])
    | Some (t, tl) -> write_func (Ctuple (t :: tl))
    | None -> write_func (Ctuple []))
  *> save_args
  *> iter (fun stmt -> cstmt stmt) anon_func.body
  *> delete_func
  *> delete_env
  *> return (get_anon_func_type anon_func)
;;

let retrieve_const caf = function
  | Const_array (size, t, _) -> return (Ctype (Type_array (size, t)))
  | Const_int _ -> return (Ctype Type_int)
  | Const_string _ -> return (Ctype Type_string)
  | Const_func anon_func -> check_anon_func anon_func caf
;;

let check_main =
  read_global_ident "main"
  >>= function
  | Some (Ctype (Type_func ([], []))) -> return ()
  | Some (Ctype (Type_func _)) ->
    fail
      (Type_check_error
         (Incorrect_main
            (Printf.sprintf "func main must have no arguments and no return values")))
  | _ -> fail (Type_check_error (Incorrect_main (Printf.sprintf "main func not found")))
;;

let eq_type t1 t2 =
  match equal_ctype t1 t2 with
  | true -> return t1
  | false -> fail (Type_check_error (Mismatched_types "Types mismatched in equation"))
;;

let check_eq t1 t2 =
  match equal_ctype t1 t2 with
  | true -> return ()
  | false -> fail (Type_check_error (Mismatched_types "Types mismatched in equation"))
;;

let check_func_call f (func, args) =
  let ftype =
    f func
    >>= function
    | Ctype (Type_func (lst, _)) -> map (fun x -> return (Ctype x)) lst
    | _ -> fail (Type_check_error (Mismatched_types "Expected func type here"))
  in
  let argtype =
    match args with
    | [ x ] ->
      f x
      >>= (function
       | Ctuple x -> map (fun x1 -> return (Ctype x1)) x
       | x -> return [ x ])
    | lst ->
      map
        (fun x ->
          f x
          >>= function
          | Ctuple _ ->
            fail (Type_check_error (Mismatched_types "Expected func type here"))
          | x -> return x)
        lst
  in
  (argtype
   >>= (fun at -> ftype >>= fun ft -> return (List.length ft = List.length at))
   >>= function
   | true -> ftype >>= fun x -> iter2 (fun arg typ -> f arg >>= check_eq typ) args x
   | false -> fail (Type_check_error (Mismatched_types "Number of arg given mismached")))
  *> return ()
;;

let rec retrieve_expr caf = function
  | Expr_const const -> retrieve_const caf const
  | Expr_un_oper (op, expr) ->
    (match op with
     | Unary_minus | Unary_plus ->
       retrieve_expr caf expr
       >>= fun t -> check_eq t (Ctype Type_int) *> return (Ctype Type_int)
     | Unary_not -> retrieve_expr caf expr)
    >>= fun t -> check_eq t (Ctype Type_bool) *> return (Ctype Type_bool)
  | Expr_ident id -> retrieve_ident id
  | Expr_bin_oper (op, left, right) ->
    let compare_arg_typ type1 type2 =
      retrieve_expr caf type1 >>= fun type1 -> retrieve_expr caf type2 >>= eq_type type1
    in
    let compare_operation_typ type1 type2 t = compare_arg_typ type1 type2 >>= eq_type t in
    (match op with
     | Bin_sum | Bin_divide | Bin_modulus | Bin_multiply | Bin_subtract ->
       compare_operation_typ left right (Ctype Type_int) *> return (Ctype Type_int)
     | Bin_less | Bin_greater | Bin_greater_equal | Bin_less_equal ->
       compare_operation_typ left right (Ctype Type_int) *> return (Ctype Type_bool)
     | Bin_or | Bin_and ->
       compare_operation_typ left right (Ctype Type_bool) *> return (Ctype Type_bool)
     | Bin_equal | Bin_not_equal -> compare_arg_typ left right *> return (Ctype Type_bool))
  | Expr_call (func, args) ->
    check_func_call (retrieve_expr caf) (func, args)
    *> map (retrieve_expr caf) args
    *> (retrieve_expr caf) func
    >>= (function
     | Ctype (Type_func (_, fst :: snd :: tl)) -> return (Ctuple (fst :: snd :: tl))
     | Ctype (Type_func (_, hd :: _)) -> return (Ctype hd)
     | _ ->
       fail (Type_check_error (Mismatched_types "Function without returns in expression")))
  | Expr_chan_receive x ->
    retrieve_expr caf x
    >>= fun x ->
    (match x with
     | Ctype (Type_chan (_, y)) -> return (Ctype y)
     | _ -> fail (Type_check_error (Mismatched_types "Chan type mismatch")))
  | Expr_index (array, index) when retrieve_expr caf index = return (Ctype Type_int) ->
    retrieve_expr caf array
    >>= (function
     | Ctype (Type_array (_, t)) -> return (Ctype t)
     | _ ->
       fail (Type_check_error (Mismatched_types "Non-array type in array index call")))
  | Expr_index (_, _) ->
    fail (Type_check_error (Mismatched_types "Array index is not int"))
;;

let check_long_var_decl caf env decl =
  match decl with
  | Long_decl_no_init (t, hd, tl) -> iter (fun i -> env i (Ctype t)) (hd :: tl)
  | Long_decl_mult_init (Some t, hd, tl) ->
    iter
      ((fun k (id, expr) ->
         (retrieve_expr caf expr >>= fun x -> check_eq x k) *> env id k)
         (Ctype t))
      (hd :: tl)
  | Long_decl_mult_init (None, hd, tl) ->
    iter (fun (id, expr) -> retrieve_expr caf expr >>= env id) (hd :: tl)
  | Long_decl_one_init (Some t, fst, snd, tl, call) ->
    retrieve_expr caf (Expr_call call)
    >>= fun x ->
    check_eq x (Ctuple (List.init (List.length (fst :: snd :: tl)) (fun _ -> t)))
    *> iter (fun i -> env i (Ctype t)) (fst :: snd :: tl)
  | Long_decl_one_init (None, fst, snd, tl, call) ->
    retrieve_expr caf (Expr_call call)
    >>= (function
     | Ctype _ ->
       fail
         (Type_check_error
            (Mismatched_types "function returns only one element in multiple var decl"))
     | Ctuple types when List.length types = List.length (fst :: snd :: tl) ->
       iter2 (fun x y -> env x y) (fst :: snd :: tl) (List.map (fun x -> Ctype x) types)
     | Ctuple _ ->
       fail
         (Type_check_error
            (Mismatched_types
               "function returns wrong number of elements in multiple var assign")))
;;

let check_short_var_decl caf = function
  | Short_decl_mult_init (hd, tl) ->
    iter (fun (id, expr) -> retrieve_expr caf expr >>= save_local_ident id) (hd :: tl)
  | Short_decl_one_init (fst, snd, tl, call) ->
    retrieve_expr caf (Expr_call call)
    >>= (function
     | Ctype _ ->
       fail
         (Type_check_error
            (Mismatched_types
               "function returns wrong number of elements in multiple var decl"))
     | Ctuple x ->
       (match List.length (fst :: snd :: tl) = List.length x with
        | true ->
          iter
            (fun (id, tp) -> save_local_ident id (Ctype tp))
            (List.combine (fst :: snd :: tl) x)
        | false ->
          fail
            (Type_check_error
               (Mismatched_types
                  "function returns wrong number of elements in multiple var decl"))))
;;

let rec retrieve_lvalue caf = function
  | Lvalue_ident id -> retrieve_ident id
  | Lvalue_array_index (Lvalue_ident array, index)
    when retrieve_expr caf index = return (Ctype Type_int) ->
    retrieve_ident array
    >>= (function
     | Ctype (Type_array (_, t)) -> return (Ctype t)
     | _ ->
       fail (Type_check_error (Mismatched_types "Non-array type in array index call")))
  | Lvalue_array_index (lvalue_array_index, index)
    when retrieve_expr caf index = return (Ctype Type_int) ->
    retrieve_lvalue caf lvalue_array_index
  | Lvalue_array_index (_, _) ->
    fail (Type_check_error (Mismatched_types "Array index is not int"))
;;

let check_assign caf = function
  | Assign_mult_expr (hd, tl) ->
    iter
      (fun (lvalue, expr) ->
        retrieve_lvalue caf lvalue
        >>= fun type1 -> retrieve_expr caf expr >>= check_eq type1)
      (hd :: tl)
  | Assign_one_expr (l1, l2, ls, w) ->
    retrieve_expr caf (Expr_call w)
    >>= (function
     | Ctype _ -> fail (Type_check_error (Cannot_assign "Multiple return assign failed"))
     | Ctuple x ->
       (match List.length x = List.length (l1 :: l2 :: ls) with
        | true ->
          iter2
            (fun x y -> retrieve_lvalue caf y >>= check_eq (Ctype x))
            x
            (l1 :: l2 :: ls)
        | false -> fail (Type_check_error (Cannot_assign "Multiple return assign failed"))))
;;

let check_init caf = function
  | Some (Init_assign assign) -> check_assign caf assign
  | Some (Init_call call) -> check_func_call (retrieve_expr caf) call
  | Some (Init_decl x) -> check_short_var_decl caf x *> return ()
  | Some (Init_decr id) -> retrieve_ident id >>= check_eq (Ctype Type_int)
  | Some (Init_incr id) -> retrieve_ident id >>= check_eq (Ctype Type_int)
  | Some (Init_receive chan) -> retrieve_expr caf chan *> return ()
  | Some (Init_send (id, expr)) ->
    retrieve_ident id *> retrieve_expr caf expr *> return ()
  | None -> return ()
;;

let rec check_stmt = function
  | Stmt_long_var_decl long_decl ->
    check_long_var_decl check_stmt save_local_ident long_decl
  | Stmt_short_var_decl short_decl -> check_short_var_decl check_stmt short_decl
  | Stmt_incr id -> retrieve_ident id >>= check_eq (Ctype Type_int)
  | Stmt_decr id -> retrieve_ident id >>= check_eq (Ctype Type_int)
  | Stmt_assign assign -> check_assign check_stmt assign
  | Stmt_call call -> check_func_call (retrieve_expr check_stmt) call
  | Stmt_defer call -> check_func_call (retrieve_expr check_stmt) call
  | Stmt_go call -> check_func_call (retrieve_expr check_stmt) call
  | Stmt_chan_send (id, expr) ->
    retrieve_expr check_stmt expr
    >>= fun x ->
    retrieve_ident id
    >>= (function
     | Ctype (Type_chan (_, t)) -> check_eq x (Ctype t)
     | _ -> fail (Type_check_error (Mismatched_types "expected chan type")) *> return ())
  | Stmt_block block -> write_env *> iter check_stmt block *> delete_env
  | Stmt_break -> return ()
  | Stmt_chan_receive chan -> retrieve_expr check_stmt chan *> return ()
  | Stmt_continue -> return ()
  | Stmt_return exprs ->
    (get_func_name
     >>= (function
            | Ctuple rtv ->
              (match List.length exprs = List.length rtv with
               | true -> return (List.combine exprs (List.map (fun x -> Ctype x) rtv))
               | false ->
                 fail (Type_check_error (Mismatched_types "func return types mismatch")))
            | _ -> fail (Type_check_error Check_failed))
     >>= iter (fun (x, y) -> retrieve_expr check_stmt x >>= check_eq y))
    *> return ()
  | Stmt_if if' ->
    write_env *> check_init check_stmt if'.init *> retrieve_expr check_stmt if'.cond
    >>= fun x ->
    check_eq (Ctype Type_bool) x
    *> iter check_stmt if'.if_body
    *> delete_env
    *>
      (match if'.else_body with
      | Some (Else_block block) -> iter check_stmt block
      | Some (Else_if if') -> check_stmt (Stmt_if if')
      | None -> return ())
  | Stmt_for { init; cond; post; body } ->
    write_env
    *> check_init check_stmt init
    *> (match cond with
      | Some x -> retrieve_expr check_stmt x >>= check_eq (Ctype Type_bool)
      | None -> return ())
    *> check_init check_stmt post
    *> iter check_stmt body
    *> delete_env
;;

let check_top_decl_funcs = function
  | Decl_func (id, args_returns_and_body) ->
    save_global_ident id (get_anon_func_type args_returns_and_body)
  | Decl_var _ -> return ()
;;

let check_top_decl = function
  | Decl_func (_, y) -> check_anon_func y check_stmt *> return ()
  | Decl_var x -> check_long_var_decl check_stmt save_global_ident x
;;

let type_check file =
  run
    (iter check_top_decl_funcs file *> iter check_top_decl file *> check_main)
    (MapIdent.empty, [], [])
;;

let pp ast =
  match type_check ast with
  | _, Result.Ok _ -> print_endline "CORRECT"
  | _, Result.Error x ->
    prerr_string "ERROR WHILE TYPECHECK WITH ";
    (match x with
     | Type_check_error Check_failed -> prerr_endline "Check failed"
     | Type_check_error (Multiple_declaration x) ->
       prerr_string ("Multiple declaration error: " ^ x)
     | Type_check_error (Incorrect_main x) -> prerr_endline ("Incorrect main error: " ^ x)
     | Type_check_error (Undefined_ident x) ->
       prerr_endline ("Undefined ident error: " ^ x)
     | Type_check_error (Mismatched_types x) -> prerr_endline ("Mismatched types: " ^ x)
     | Type_check_error (Cannot_assign x) -> prerr_endline ("Mismatched types: " ^ x))
;;
