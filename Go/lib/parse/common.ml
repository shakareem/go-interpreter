(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Angstrom

let pp printer parser str =
  match Angstrom.parse_string ~consume:Angstrom.Consume.All parser str with
  | Ok res -> printer Format.std_formatter res
  | Error res -> print_endline res
;;

let is_keyword = function
  (* https://go.dev/ref/spec#Keywords *)
  | "break"
  | "case"
  | "chan"
  | "const"
  | "defer"
  | "else"
  | "for"
  | "func"
  | "go"
  | "if"
  | "range"
  | "return"
  | "type"
  | "var" -> true
  | _ -> false
;;

let skip_whitespace = skip_many1 (satisfy Char.is_whitespace)

let skip_line_whitespace =
  skip_many1
    (satisfy (fun c ->
       match c with
       | ' ' | '\t' -> true
       | _ -> false))
;;

let parse_line_comment = string "//" *> many_till any_char (char '\n') *> return ()
let parse_block_comment = string "/*" *> many_till any_char (string "*/") *> return ()
let parse_comment = parse_line_comment <|> parse_block_comment
let ws = skip_many (parse_comment <|> skip_whitespace)
let ws_line = skip_many (parse_block_comment <|> skip_line_whitespace)
let token s = ws_line *> string s <* ws
let parens p = char '(' *> ws *> p <* ws_line <* char ')'
let square_brackets p = char '[' *> ws *> p <* ws_line <* char ']'
let curly_braces p = char '{' *> ws *> p <* ws_line <* char '}'
let parse_stmt_sep = ws_line *> char '\n' *> ws <|> ws_line *> char ';' *> ws
let parse_int = take_while1 Char.is_digit >>| fun num -> Int.of_string num

let parse_const_int =
  take_while1 Char.is_digit >>| fun num -> Const_int (Int.of_string num)
;;

let parse_const_string =
  let parse_string = take_till (Char.equal '"') >>| fun str -> Const_string str in
  char '"' *> parse_string <* char '"'
;;

(* TODO: add const*)
let parse_const = parse_const_int <|> parse_const_string

let parse_ident =
  let is_first_char_valid = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false
  in
  let is_valid_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  let* first_char = peek_char in
  match first_char with
  | Some chr when is_first_char_valid chr ->
    let* ident = take_while is_valid_char in
    if is_keyword ident then fail "This is a keyword" else return ident
  | _ -> fail "Invalid ident"
;;

let parse_ident_not_blank =
  let* ident = parse_ident in
  match ident with
  | "_" -> fail "Blank identifier is a write-only value"
  | _ -> return ident
;;

let parse_simple_type =
  choice
    [ string "int" *> return Type_int
    ; string "string" *> return Type_string
    ; string "bool" *> return Type_bool
    ]
    ~failure_msg:"Invalid type"
;;

(* TODO: add arrays and functions *)
let parse_type =
  fix (fun type' ->
    let parse_array_type =
      lift2
        (fun size type' -> Type_array (type', size))
        (square_brackets parse_int)
        (ws *> type')
    in
    let arg = parse_array_type <|> parse_simple_type in
    let arg = fix (fun _ -> arg) <|> arg in
    arg)
;;

(**************************************** Tests ****************************************)

let%expect_test "const int" =
  pp pp_const parse_const {|256|};
  [%expect {| (Const_int 256) |}]
;;

let%expect_test "zero" =
  pp pp_const parse_const {|0|};
  [%expect {| (Const_int 0) |}]
;;

let%expect_test "not digit in int" =
  pp pp_const parse_const {|123,321|};
  [%expect {| : end_of_input |}]
;;

(* bug
let%expect_test "very big int" =
  pp pp_const parse_const {|9999999999999999999999999999999999999999|};
  [%expect {||}]
;; *)

let%expect_test "const string" =
  pp pp_const parse_const {|"my_string"|};
  [%expect {| (Const_string "my_string") |}]
;;

let%expect_test "string with '\n'" =
  pp pp_const parse_const {|"Hello\n"|};
  [%expect {| (Const_string "Hello\\n") |}]
;;

let%expect_test "ident with only letters" =
  pp pp_ident parse_ident {|myIdent|};
  [%expect {| "myIdent" |}]
;;

let%expect_test "ident with uderscore" =
  pp pp_ident parse_ident {|my_ident|};
  [%expect {| "my_ident" |}]
;;

let%expect_test "blank ident" =
  pp pp_ident parse_ident {|_|};
  [%expect {| "_" |}]
;;

let%expect_test "ident with numbers" =
  pp pp_ident parse_ident {|a1b2c3|};
  [%expect {| "a1b2c3" |}]
;;

let%expect_test "ident with first char that is digit" =
  pp pp_ident parse_ident {|1abc|};
  [%expect {| : Invalid ident |}]
;;

let%expect_test "not blank ident" =
  pp pp_ident parse_ident_not_blank {|abcdefg|};
  [%expect {| "abcdefg" |}]
;;

let%expect_test "not blank ident with blank input" =
  pp pp_ident parse_ident_not_blank {|_|};
  [%expect {| : Blank identifier is a write-only value |}]
;;

let%expect_test "incorrect type" =
  pp pp_type' parse_type {|blablablablabla|};
  [%expect {| : Invalid type |}]
;;

let%expect_test "type int" =
  pp pp_type' parse_type {|int|};
  [%expect {| Type_int |}]
;;

let%expect_test "type bool" =
  pp pp_type' parse_type {|bool|};
  [%expect {| Type_bool |}]
;;

let%expect_test "type string" =
  pp pp_type' parse_type {|string|};
  [%expect {| Type_string |}]
;;

(* bug *)
let%expect_test "type array of arrays" =
  pp pp_type' parse_type {|[4][0]string|};
  [%expect {| (Type_array ((Type_array (Type_string, 0)), 4)) |}]
;;

(* bug *)
let%expect_test "type array of functions" =
  pp pp_type' parse_type {|[4]func()|};
  [%expect {| : Invalid type |}]
;;

(* bug *)
let%expect_test "type simple func" =
  pp pp_type' parse_type {|func()|};
  [%expect {| : Invalid type |}]
;;

(* bug *)
let%expect_test "type simple func with brackets" =
  pp pp_type' parse_type {|func()()|};
  [%expect {| : Invalid type |}]
;;

(* bug *)
let%expect_test "type simple func with brackets and ws" =
  pp pp_type' parse_type {|func()  /* some comment */  ()|};
  [%expect {| : Invalid type |}]
;;

(* bug *)
let%expect_test "type func with one arg and without returns" =
  pp pp_type' parse_type {|func(int)|};
  [%expect {| : Invalid type |}]
;;

(* bug *)
let%expect_test "type func with mult args and without returns" =
  pp pp_type' parse_type {|func(int, string, bool, [4]int)|};
  [%expect {| : Invalid type |}]
;;

(* bug *)
let%expect_test "type func with one return" =
  pp pp_type' parse_type {|func() int|};
  [%expect {| : Invalid type |}]
;;

(* bug *)
let%expect_test "type func with multiple returns" =
  pp pp_type' parse_type {|func() (int, string)|};
  [%expect {| : Invalid type |}]
;;

(* bug *)
let%expect_test "type func that gets func and returns func" =
  pp pp_type' parse_type {|func(func(int) string) func([4][5]int)|};
  [%expect {| : Invalid type |}]
;;

(* bug *)
let%expect_test "type func that returns func that returns func..." =
  pp pp_type' parse_type {|func() func() func() func() func() func()|};
  [%expect {| : Invalid type |}]
;;

let%expect_test "type int" =
  pp pp_type' parse_type {|int|};
  [%expect {| Type_int |}]
;;

let%expect_test "type bool" =
  pp pp_type' parse_type {|bool|};
  [%expect {| Type_bool |}]
;;

let%expect_test "type string" =
  pp pp_type' parse_type {|string|};
  [%expect {| Type_string |}]
;;

(* bug *)
let%expect_test "type array of arrays" =
  pp pp_type' parse_type {|[4][0]string|};
  [%expect {| (Type_array ((Type_array (Type_string, 0)), 4)) |}]
;;

(* bug *)
let%expect_test "type array of functions" =
  pp pp_type' parse_type {|[4]func()|};
  [%expect {| : Invalid type |}]
;;

(* bug *)
let%expect_test "type simple func" =
  pp pp_type' parse_type {|func()|};
  [%expect {| : Invalid type |}]
;;

(* bug *)
let%expect_test "type simple func with brackets" =
  pp pp_type' parse_type {|func()()|};
  [%expect {| : Invalid type |}]
;;

(* bug *)
let%expect_test "type simple func with brackets and ws" =
  pp pp_type' parse_type {|func()  /* some comment */  ()|};
  [%expect {| : Invalid type |}]
;;

(* bug *)
let%expect_test "type func with one arg and without returns" =
  pp pp_type' parse_type {|func(int)|};
  [%expect {| : Invalid type |}]
;;

(* bug *)
let%expect_test "type func with mult args and without returns" =
  pp pp_type' parse_type {|func(int, string, bool, [4]int)|};
  [%expect {| : Invalid type |}]
;;

(* bug *)
let%expect_test "type func with one return" =
  pp pp_type' parse_type {|func() int|};
  [%expect {| : Invalid type |}]
;;

(* bug *)
let%expect_test "type func with multiple returns" =
  pp pp_type' parse_type {|func() (int, string)|};
  [%expect {| : Invalid type |}]
;;

(* bug *)
let%expect_test "type func that gets func and returns func" =
  pp pp_type' parse_type {|func(func(int) string) func([4][5]int)|};
  [%expect {| : Invalid type |}]
;;

(* barray_ug *)
let%expect_test "type func that returns func that returns func..." =
  pp pp_type' parse_type {|func() func() func() func() func() func()|};
  [%expect {| : Invalid type |}]
;;