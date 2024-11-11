(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open QCheck.Gen
open Ast

(* надо запомнить про опшн *)

let gen_ident_first_char = oneof [ char_range 'a' 'b'; char_range 'A' 'B'; return '_' ]

let gen_ident_char =
  oneof [ char_range 'a' 'b'; char_range 'A' 'B'; return '_'; char_range '0' '9' ]
;;

let gen_ident =
  let* first_char = gen_ident_first_char in
  let* rest_chars = string_size ~gen:gen_ident_char (int_range 0 9) in
  return (Char.to_string first_char ^ rest_chars)
;;
