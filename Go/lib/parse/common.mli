(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Angstrom

val skip_whitespase: unit t

(** Separator for the statements, [;] or [\n] *)
val parse_stmt_sep: char t 

val parse_const: const t

val parse_ident: string t

val parse_type: type' t