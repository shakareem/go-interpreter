(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open AstGenerator
open AstShrinker
open Pprinter.Printer
open Parse

let arbitrary_file_manual = QCheck.make gen_file ~shrink:shrink_file ~print:print_file

let manual_test =
  QCheck.(
    Test.make ~name:"QCheck test" ~count:10 arbitrary_file_manual (fun file ->
      Result.ok file = parse parse_file (print_file file)))
;;

QCheck_base_runner.run_tests_main [ manual_test ]
