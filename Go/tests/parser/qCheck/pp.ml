(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Printf

let parens f =
  printf "( ";
  f;
  printf ") "
;;

let brackets f =
  printf "[ ";
  f;
  printf "] "
;;

let print_bin_op = function
  | Bin_sum -> printf "+"
  | Bin_multiply -> printf "*"
  | Bin_subtract -> printf "-"
  | Bin_divide -> printf "/"
  | Bin_modulus -> printf "%%"
  | Bin_equal -> printf "=="
  | Bin_not_equal -> printf "!="
  | Bin_greater -> printf ">"
  | Bin_greater_equal -> printf ">="
  | Bin_less -> printf "<"
  | Bin_less_equal -> printf "<="
  | Bin_and -> printf "&&"
  | Bin_or -> printf " ||"
;;

let rec print_type type' =
  match type' with
  | Type_int -> printf "int"
  | Type_string -> printf "string"
  | Type_bool -> printf "bool"
  | Type_array (int, type') ->
    printf "[%i]" int;
    print_type type'
  | Type_func (lst1, lst2) ->
    printf "func(";
    List.iter print_type lst1;
    printf ") (";
    List.iter print_type lst2;
    printf ") "
  | Type_chan chan_type ->
    (match chan_type with
     | Chan_bidirectional t ->
       printf "chan ";
       print_type t
     | Chan_receive t ->
       printf "<- chan ";
       print_type t
     | Chan_send t ->
       printf "chan<- ";
       print_type t)
;;

let print_pair_type t =
  let t1, t2 = List.split t in
  List.iter2
    (fun a b ->
      printf "%s" a;
      print_type b)
    t1
    t2
;;

let print_un_op = function
  | Unary_not -> printf " !"
  | Unary_plus -> printf " +"
  | Unary_minus -> printf " -"
  | Unary_recieve -> printf " <-"
;;

let print_return_values = function
  | Only_types t -> parens (List.iter print_type t)
  | Ident_and_types t -> parens (print_pair_type t)
;;

let print_func_call t f =
  let e1, el = t in
  f e1;
  List.iter f el
;;

let rec print_expr expr =
  match expr with
  | Expr_const (Const_int num) -> printf "%i" num
  | Expr_const (Const_string str) -> printf "%s" str
  | Expr_const (Const_array (intt, type', expr_lst)) ->
    printf "[%i]" intt;
    print_type type';
    printf "{";
    List.iter print_expr expr_lst;
    printf "}"
  | Expr_const (Const_func afunc) ->
    printf "func";
    print_pair_type afunc.args;
    (match afunc.returns with
     | Some x -> parens (print_return_values x) (*print block*)
     | None -> printf " ")
  | Expr_ident idnt -> printf "%s" idnt
  | Expr_index (exp1, exp2) ->
    print_expr exp1;
    brackets (print_expr exp2)
  | Expr_bin_oper (t, arg1, arg2) ->
    print_expr arg1;
    print_bin_op t;
    print_expr arg2
  | Expr_un_oper (t, arg1) ->
    print_un_op t;
    print_expr arg1
  | Expr_call fc -> print_func_call fc print_expr
;;
