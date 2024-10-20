(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Angstrom

val pp : (Format.formatter -> 'a -> unit) -> 'a t -> ident -> unit
val ws : unit t
val ws_line : unit t
val token : string -> string t
val parens : 'a t -> 'a t
val square_brackets : 'a t -> 'a t
val curly_braces : 'a t -> 'a t
val parse_int : size t

(** Parses separator for the statements, [;] or [\n], returns nothing *)
val parse_stmt_sep : unit t

val parse_const : const t

(** Parses identifiers that can be used as lvalues in variable declarations
    and as function name (includes blank identifier [_]) *)
val parse_ident : ident t

(** Parses identifiers that are not blank ([_]) *)
val parse_ident_not_blank : ident t

val parse_type : type' t
