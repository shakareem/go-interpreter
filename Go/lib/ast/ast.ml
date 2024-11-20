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
    small_string
      ~gen:
        (oneof [ char_range 'a' 'z'; char_range 'A' 'Z'; return '_'; char_range '0' '9' ])
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
[@@deriving show { with_path = false }, qcheck]

type 'stmt blck = 'stmt list [@@deriving show { with_path = false }]

type 'stmt afnc =
  { args : (ident * type') list
  ; returns : return_values option
  ; body : 'stmt blck
  }
[@@deriving show { with_path = false }]

type ('exp, 'stmt) cnst =
  | Const_int of int
  | Const_string of string
  | Const_array of int * type' * 'exp list
  | Const_func of 'stmt blck afnc
[@@deriving show { with_path = false }]

type 'exp fcall = 'exp * 'exp list [@@deriving show { with_path = false }]
type 'exp recv = 'exp [@@deriving show { with_path = false }]

type 'stmt exp =
  | Expr_const of ('stmt blck exp, 'stmt blck) cnst
  | Expr_ident of ident
  | Expr_index of 'stmt blck exp * 'stmt blck exp
  | Expr_bin_oper of bin_oper * 'stmt blck exp * 'stmt blck exp
  | Expr_un_oper of unary_oper * 'stmt blck exp
  | Expr_recv of 'stmt blck exp recv
  | Expr_call of 'stmt blck exp fcall
[@@deriving show { with_path = false }]

type 'exp send = ident * 'exp [@@deriving show { with_path = false }]

type 'exp lvlue =
  | Lvalue_ident of ident
  | Lvalue_array_index of 'exp lvlue * 'exp
[@@deriving show { with_path = false }]

type 'exp asgn =
  | Assign_mult_exp of ('exp lvlue * 'exp) * ('exp lvlue * 'exp) list
  | Assign_one_exp of 'exp lvlue * 'exp lvlue * 'exp lvlue list * 'exp fcall
[@@deriving show { with_path = false }]

type 'exp ldcl =
  | Long_decl_no_init of type' * ident * ident list
  | Long_decl_mult_init of type' option * (ident * 'exp) * (ident * 'exp) list
  | Long_decl_one_init of type' option * ident * ident * ident list * 'exp fcall
[@@deriving show { with_path = false }]

type 'exp sdcl =
  | Short_decl_mult_init of (ident * 'exp) * (ident * 'exp) list
  | Short_decl_one_init of ident * ident * ident list * 'exp fcall
[@@deriving show { with_path = false }]

type 'exp init =
  | Init_asgn of 'exp asgn
  | Init_decl of 'exp sdcl
  | Init_incr of ident
  | Init_decr of ident
  | Init_call of 'exp fcall
  | Init_send of 'exp send
  | Init_receive of 'exp recv
[@@deriving show { with_path = false }]

type ('stmt, 'elsb) iff =
  { init : 'stmt blck exp init option
  ; cond : 'stmt blck exp
  ; if_body : 'stmt blck
  ; elsb : 'elsb option
  }
[@@deriving show { with_path = false }]

type 'stmt elsb =
  | Else_block of 'stmt blck
  | Else_if of ('stmt blck, 'stmt blck elsb) iff
[@@deriving show { with_path = false }]

type stmt =
  | Stmt_ldcl of stmt blck exp ldcl
  | Stmt_sdcl of stmt blck exp sdcl
  | Stmt_asgn of stmt blck exp asgn
  | Stmt_incr of ident
  | Stmt_decr of ident
  | Stmt_break
  | Stmt_continue
  | Stmt_return of stmt blck exp list
  | Stmt_block of stmt blck
  | Stmt_send of stmt blck exp send
  | Stmt_recv of stmt blck exp recv
  | Stmt_call of stmt blck exp fcall
  | Stmt_defer of stmt blck exp fcall
  | Stmt_go of stmt blck exp fcall
  | Stmt_if of (stmt blck, stmt blck elsb) iff
  | Stmt_for of
      { init : stmt blck exp init option
      ; cond : stmt blck exp option
      ; post : stmt blck exp init option
      ; body : stmt blck
      }
[@@deriving show { with_path = false }]

type block = stmt blck [@@deriving show { with_path = false }]
type anon_func = stmt afnc [@@deriving show { with_path = false }]
type expr = stmt exp [@@deriving show { with_path = false }]
type const = (expr, stmt) cnst [@@deriving show { with_path = false }]
type func_call = expr fcall [@@deriving show { with_path = false }]
type chan_receive = expr recv [@@deriving show { with_path = false }]
type chan_send = expr send [@@deriving show { with_path = false }]
type lvalue = expr lvlue [@@deriving show { with_path = false }]
type assign = expr asgn [@@deriving show { with_path = false }]
type long_var_decl = expr ldcl [@@deriving show { with_path = false }]
type short_var_decl = expr sdcl [@@deriving show { with_path = false }]
type if_for_init = expr init [@@deriving show { with_path = false }]
type else_body = stmt elsb [@@deriving show { with_path = false }]
type if' = (stmt, else_body) iff [@@deriving show { with_path = false }]
type func_decl = ident * block afnc [@@deriving show { with_path = false }]

type top_decl =
  | Decl_var of block exp ldcl
  | Decl_func of func_decl
[@@deriving show { with_path = false }]

type file = top_decl list [@@deriving show { with_path = false }]
