(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open QCheck.Gen
open Ast

(*
   надо запомнить про опшн
   small_nat
*)

let int5 = int_range 0 5
let int1_5 = int_range 1 5
let list5 : 'a t -> 'a list t = list_size int5
let list1_5 : 'a t -> 'a list t = list_size int1_5
let gen_ident_first_char = oneof [ char_range 'a' 'b'; char_range 'A' 'B'; return '_' ]

let gen_ident_char =
  oneof [ char_range 'a' 'b'; char_range 'A' 'B'; return '_'; char_range '0' '9' ]
;;

let gen_ident =
  let* first_char = gen_ident_first_char in
  let* rest_chars = string_size ~gen:gen_ident_char int5 in
  return (Char.to_string first_char ^ rest_chars)
;;

(********** types **********)

let gen_array_type gtype =
  let* size = int5 in
  let* type' = gtype in
  return (Type_array (size, type'))
;;

let gen_func_type gtype =
  let* arg_types = list5 gtype in
  let* return_types = list5 gtype in
  return (Type_func (arg_types, return_types))
;;

let gen_chan_type gtype =
  let* type' = gtype in
  let* chan_type =
    oneofl [ Chan_bidirectional type'; Chan_receive type'; Chan_send type' ]
  in
  return (Type_chan chan_type)
;;

let gen_type =
  fix
    (fun _ gtype ->
      oneof
        [ return Type_int
        ; return Type_string
        ; return Type_bool
        ; gen_array_type gtype
        ; gen_func_type gtype
        ; gen_chan_type gtype
        ])
    (return Type_int)
;;

(********** const **********)

let gen_const_int =
  let* num = int_range 0 1000 in
  return (Const_int num)
;;

let gen_const_string =
  let* str = string_size int5 in
  return (Const_string str)
;;

let gen_const_array gexpr =
  let* size = int5 in
  let* type' = gen_type in
  let* inits = list_size int5 gexpr in
  return (Const_array (size, type', inits))
;;

let gen_return_values =
  let gen_only_types =
    let* types = list1_5 gen_type in
    return (Only_types types)
  in
  let gen_ident_and_types =
    let* idents_and_types = list1_5 (pair gen_ident gen_type) in
    return (Ident_and_types idents_and_types)
  in
  oneof [ gen_only_types; gen_ident_and_types ]
;;

let gen_anon_func gblock =
  let* args = list1_5 (pair gen_ident gen_type) in
  let* returns = option gen_return_values in
  let* body = gblock in
  return { args; returns; body }
;;

let gen_const_func gblock =
  let* anon_func = gen_anon_func gblock in
  return (Const_func anon_func)
;;

let gen_const gexpr gblock =
  oneof [ gen_const_int; gen_const_string; gen_const_array gexpr; gen_const_func gblock ]
;;

(********** expr **********)

let gen_bin_op =
  oneofl
    [ Bin_sum
    ; Bin_multiply
    ; Bin_subtract
    ; Bin_divide
    ; Bin_modulus
    ; Bin_equal
    ; Bin_not_equal
    ; Bin_greater
    ; Bin_greater_equal
    ; Bin_less
    ; Bin_less_equal
    ; Bin_and
    ; Bin_or
    ]
;;

let gen_un_op = oneofl [ Unary_not; Unary_plus; Unary_minus; Unary_recieve ]
