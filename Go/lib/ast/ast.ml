(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open QCheck.Gen

let sized4 st = sized_size (int_range 0 4) st

type chan_dir =
  | Chan_bidirectional
  | Chan_receive
  | Chan_send
[@@deriving show { with_path = false }, qcheck]

type type' =
  | Type_int
  | Type_string
  | Type_bool
  | Type_array of int * type'
  | Type_func of type' list * type' list
  | Type_chan of chan_dir * type'
[@@deriving show { with_path = false }, qcheck]

let gen_type' = sized4 gen_type'_sized

let gen_ident =
  let open QCheck.Gen in
  let is_keyword = function
    | "break"
    | "func"
    | "defer"
    | "go"
    | "chan"
    | "if"
    | "else"
    | "continue"
    | "for"
    | "return"
    | "var" -> true
    | _ -> false
  in
  let* first_char = oneof [ char_range 'a' 'z'; char_range 'A' 'Z'; return '_' ] in
  let* rest_chars =
    string_size
      ~gen:
        (oneof [ char_range 'a' 'z'; char_range 'A' 'Z'; return '_'; char_range '0' '9' ])
      (int_range 0 9)
  in
  let ident = Base.Char.to_string first_char ^ rest_chars in
  return (if is_keyword ident then "_" ^ ident else ident)
;;

type ident = (string[@gen gen_ident]) [@@deriving show { with_path = false }]

type bin_oper =
  | Bin_sum
  | Bin_multiply
  | Bin_subtract
  | Bin_divide
  | Bin_modulus
  | Bin_equal
  | Bin_not_equal
  | Bin_greater
  | Bin_greater_equal
  | Bin_less
  | Bin_less_equal
  | Bin_and
  | Bin_or
[@@deriving show { with_path = false }, qcheck]

type unary_oper =
  | Unary_not
  | Unary_plus
  | Unary_minus
[@@deriving show { with_path = false }, qcheck]

type return_values =
  | Only_types of type' * type' list
  | Ident_and_types of (ident * type') * (ident * type') list
[@@deriving show { with_path = false }]

type expr =
  | Expr_const of const
  | Expr_ident of ident
  | Expr_index of expr * expr
  | Expr_bin_oper of bin_oper * expr * expr
  | Expr_un_oper of unary_oper * expr
  | Expr_chan_receive of chan_receive
  | Expr_call of func_call
[@@deriving show { with_path = false }]

and const =
  | Const_int of int
  | Const_string of string
  | Const_array of int * type' * expr list
  | Const_func of anon_func
[@@deriving show { with_path = false }]

and anon_func =
  { args : (ident * type') list
  ; returns : return_values option
  ; body : block
  }
[@@deriving show { with_path = false }]

and func_call = expr * expr list [@@deriving show { with_path = false }]
and chan_receive = expr [@@deriving show { with_path = false }]
and chan_send = ident * expr [@@deriving show { with_path = false }]

and lvalue =
  | Lvalue_ident of ident
  | Lvalue_array_index of lvalue * expr
[@@deriving show { with_path = false }]

and assign =
  | Assign_mult_expr of (lvalue * expr) * (lvalue * expr) list
  | Assign_one_expr of lvalue * lvalue * lvalue list * func_call
[@@deriving show { with_path = false }]

and long_var_decl =
  | Long_decl_no_init of type' * ident * ident list
  | Long_decl_mult_init of type' option * (ident * expr) * (ident * expr) list
  | Long_decl_one_init of type' option * ident * ident * ident list * func_call
[@@deriving show { with_path = false }]

and short_var_decl =
  | Short_decl_mult_init of (ident * expr) * (ident * expr) list
  | Short_decl_one_init of ident * ident * ident list * func_call
[@@deriving show { with_path = false }]

and if_for_init =
  | Init_assign of assign
  | Init_decl of short_var_decl
  | Init_incr of ident
  | Init_decr of ident
  | Init_call of func_call
  | Init_send of chan_send
  | Init_receive of chan_receive
[@@deriving show { with_path = false }]

and if' =
  { init : if_for_init option
  ; cond : expr
  ; if_body : block
  ; else_body : else_body option
  }
[@@deriving show { with_path = false }]

and else_body =
  | Else_block of block
  | Else_if of if'
[@@deriving show { with_path = false }]

and stmt =
  | Stmt_long_var_decl of long_var_decl
  | Stmt_short_var_decl of short_var_decl
  | Stmt_assign of assign
  | Stmt_incr of ident
  | Stmt_decr of ident
  | Stmt_break
  | Stmt_continue
  | Stmt_return of expr list
  | Stmt_block of block
  | Stmt_chan_send of chan_send
  | Stmt_chan_receive of chan_receive
  | Stmt_call of func_call
  | Stmt_defer of func_call
  | Stmt_go of func_call
  | Stmt_if of if'
  | Stmt_for of
      { init : if_for_init option
      ; cond : expr option
      ; post : if_for_init option
      ; body : block
      }
[@@deriving show { with_path = false }]

and block = stmt list [@@deriving show { with_path = false }]

type func_decl = ident * anon_func [@@deriving show { with_path = false }]

type top_decl =
  | Decl_var of long_var_decl
  | Decl_func of func_decl
[@@deriving show { with_path = false }]

type file = top_decl list [@@deriving show { with_path = false }]
