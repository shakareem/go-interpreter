(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

type type_check_error =
  | Check_failed
  | Incorrect_main of string
  | Multiple_declaration of string
  | Undefined_ident of string
  | Mismatched_types of string
  | Cannot_assign of string
  | Missing_return of string
  | Invalid_operation of string
[@@deriving show { with_path = false }]

type devonly_runtime_error =
  | Not_enough_stack_frames
  | Not_enough_local_envs
  | Not_enough_operands
  | No_goroutine_running
  | Two_goroutine_running
  | Undefined_ident of string
  | TypeCheckFailed
[@@deriving show { with_path = false }]

type runtime_error =
  | Stack_overflow
  | Division_by_zero
  | Array_index_out_of_bound
  | Negative_array_index
  | Uninited_func
  | Deadlock
  | Panic of string
  | DevOnly of devonly_runtime_error
[@@deriving show { with_path = false }]

type error =
  | Type_check_error of type_check_error
  | Runtime_error of runtime_error
[@@deriving show { with_path = false }]