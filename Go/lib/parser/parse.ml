(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Common
open Expr
open Stmt
open TopLevel

let parse_ident = parse_ident
let parse_type = parse_type
let parse_expr = parse_expr parse_block
let parse_stmt = parse_stmt parse_block
let parse_file = parse_file
let parse parser str = Angstrom.parse_string ~consume:Angstrom.Consume.All parser str
