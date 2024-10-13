(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

let pp printer parser str =
  match Angstrom.parse_string ~consume:Angstrom.Consume.All parser str with
  | Ok res ->
      printer Format.std_formatter res
  | Error _ ->
      print_endline "Error: Wrong Syntax"