(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast

type type_check_error =
  | Check_failed
  | Not_found
  | Incorrect_main of ident
  | Multiple_declaration of ident
  | Undefined_ident of ident
  | Mismatched_types of ident
  | Unexpected_arguments of ident
  | Missing_argument of type'
  | Index_out_of_bounds of int
  | Cannot_assign of ident
[@@deriving show { with_path = false }]
