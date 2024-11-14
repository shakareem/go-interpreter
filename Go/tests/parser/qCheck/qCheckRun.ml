(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open AstGenerator
open Pprinter.Printer
open Parse
open Format

let print_file fmt file = fprintf fmt "%s" (print_file file)
let arbitrary_file = QCheck.make gen_file ~print:(Format.asprintf "%a" print_file);;

QCheck_base_runner.run_tests_main
  [ QCheck.(
      Test.make ~name:"QCheck test" ~count:10 arbitrary_file (fun file ->
        Result.ok file = parse parse_file (Format.asprintf "%a" print_file file)))
  ]
