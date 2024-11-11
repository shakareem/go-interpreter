(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open AstGenerator
open Pp
open Parser
open QCheck

(* tmp *)
let gen = gen_expr (gen_block gen_stmt)
let print_file fmt = print_expr fmt

let pprint_to_string input =
  Format.fprintf Format.str_formatter "%a" print_expr input;
  Format.flush_str_formatter ()
;;

let parse_file = Parse.parse (Expr.parse_expr Stmt.parse_block)

let arbitrary_file =
  QCheck.make ~print:(fun ast -> Format.asprintf "%a" (print_file ast)) gen
;;

let run_tests () =
  QCheck_runner.run_tests
    [ QCheck.(
        Test.make ~count:10 arbitrary_file (fun file ->
          file = parse_file (Format.asprintf "%a" print_file file)))
    ]
;;

let () =
  let _ = run_tests in
  ()
;;
