(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Angstrom
open Common

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a
let penot expr = token "!" *> expr >>= fun expr -> return @@ Expr_un_oper (Unary_not, expr)

let peusb expr =
  token "-" *> expr >>= fun expr -> return @@ Expr_un_oper (Unary_minus, expr)
;;

let pesum = token "+" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_sum, exp1, exp2))

let pemul =
  token "*" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_multiply, exp1, exp2))
;;

let pesub =
  token "-" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_subtract, exp1, exp2))
;;

let pediv = token "/" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_divide, exp1, exp2))
let pemod = token "%" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_modulus, exp1, exp2))
let peeql = token "==" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_equal, exp1, exp2))

let penql =
  token "!= " *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_not_equal, exp1, exp2))
;;

let pegrt = token ">" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_greater, exp1, exp2))

let pegre =
  token ">=" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_greater_equal, exp1, exp2))
;;

let pelss = token "<" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_less, exp1, exp2))

let pelse =
  token "<=" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_less_equal, exp1, exp2))
;;

let peand = token "&&" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_and, exp1, exp2))
let peor = token "||" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_or, exp1, exp2))

let parse_simple_expr =
  parse_ident_not_blank
  >>| (fun id -> Expr_ident id)
  <|> (parse_const >>| fun const -> Expr_const const)
;;

let parse_func_call pexpr : func_call t =
  let* func_name = ws *> parse_ident <* ws_line in
  let* args = parens (sep_by (ws_line *> char ',' *> ws) pexpr) in
  return (Expr_ident func_name, args)
;;

let parse_expr_func_call pexpr = parse_func_call pexpr >>| fun call -> Expr_call call

let parse_idents_with_types =
  let* args_lists =
    sep_by1
      (ws_line *> char ',' *> ws)
      (let* idents = sep_by1 (ws_line *> char ',' *> ws) parse_ident in
       let* t = ws_line *> parse_type in
       return (List.map ~f:(fun id -> id, t) idents))
  in
  return (List.concat args_lists)
;;

let parse_func_args = parens parse_idents_with_types <|> (parens ws >>| fun _ -> [])

let parse_func_return_values =
  choice
    [ (parens parse_idents_with_types >>| fun returns -> Some (Ident_and_types returns))
    ; (parens (sep_by1 (ws_line *> char ',' *> ws) parse_type)
       >>| fun types -> Some (Only_types types))
    ; (parse_type >>| fun t -> Some (Only_types [ t ]))
    ; (let* _ = parens ws <|> return () in
       let* char = ws_line *> peek_char_fail in
       match char with
       | '{' -> return None
       | _ -> fail "Incorrect func return values")
    ]
;;

let parse_func_args_returns_and_body pblock =
  let* args = parse_func_args <* ws_line in
  let* returns = parse_func_return_values <* ws_line in
  let* body = pblock in
  return { args; returns; body }
;;

let parse_anon_func pblock =
  string "func" *> ws *> parse_func_args_returns_and_body pblock
  >>| fun anon_func -> Expr_anon_func anon_func
;;

let parse_expr =
  fix (fun pexpr ->
    let arg =
      parens pexpr <|> parse_expr_func_call pexpr <|> parse_simple_expr (* <|> parse_anon_func pblock *)
    in  
    let arg = penot arg <|> arg in
    let arg = peusb arg <|> arg in
    let arg = chainl1 arg (pemul <|> pemod <|> pediv) in
    let arg = chainl1 arg (pesum <|> pesub) in
    let arg = chainl1 arg (pegre <|> pelse <|> pegrt <|> pelss <|> peeql <|> penql) in
    let arg = chainr1 arg peand in
    let arg = chainr1 arg peor in
    let arg = fix (fun _ -> arg) <|> arg in
    arg)
;;

(**************************************** Tests ****************************************)

let%expect_test "expr const int" =
  pp pp_expr parse_expr {|123|};
  [%expect {|
    (Expr_const (Const_int 123))|}]
;;

let%expect_test "expr const string" =
  pp pp_expr parse_expr {|"My_string123"|};
  [%expect {|
    (Expr_const (Const_string "My_string123"))|}]
;;

let%expect_test "expr ident true" =
  pp pp_expr parse_expr {|true|};
  [%expect {|
    (Expr_ident "true")|}]
;;

let%expect_test "expr ident false" =
  pp pp_expr parse_expr {|false|};
  [%expect {|
    (Expr_ident "false")|}]
;;

let%expect_test "expr ident nil" =
  pp pp_expr parse_expr {|nil|};
  [%expect {|
    (Expr_ident "nil")|}]
;;

let%expect_test "expr ident" =
  pp pp_expr parse_expr {|abcdefg__|};
  [%expect {|
    (Expr_ident "abcdefg__")|}]
;;

let%expect_test "expr ident in braces" =
  pp pp_expr parse_expr {|(abc)|};
  [%expect {|
    (Expr_ident "abc")|}]
;;

let%expect_test "expr logical operations" =
  pp pp_expr parse_expr {|a && (b || c)|};
  [%expect {|
    (Expr_bin_oper (Bin_and, (Expr_ident "a"),
       (Expr_bin_oper (Bin_or, (Expr_ident "b"), (Expr_ident "c")))))|}]
;;

let%expect_test "expr bin mult and sum" =
  pp pp_expr parse_expr {|-5 * _r + 8|};
  [%expect
    {|
    (Expr_bin_oper (Bin_sum,
       (Expr_bin_oper (Bin_multiply,
          (Expr_un_oper (Unary_minus, (Expr_const (Const_int 5)))),
          (Expr_ident "_r"))),
       (Expr_const (Const_int 8))))|}]
;;

let%expect_test "expr_call test" =
  pp pp_expr parse_expr "fac(4 + fac(4 + 4))";
  [%expect
    {|
    (Expr_call
       ((Expr_ident "fac"),
        [(Expr_bin_oper (Bin_sum, (Expr_const (Const_int 4)),
            (Expr_call
               ((Expr_ident "fac"),
                [(Expr_bin_oper (Bin_sum, (Expr_const (Const_int 4)),
                    (Expr_const (Const_int 4))))
                  ]))
            ))
          ]))|}]
;;

let%expect_test "fac_piece1 test" =
  pp pp_expr parse_expr "n * fac(n-1)";
  [%expect
    {|
    (Expr_bin_oper (Bin_multiply, (Expr_ident "n"),
       (Expr_call
          ((Expr_ident "fac"),
           [(Expr_bin_oper (Bin_subtract, (Expr_ident "n"),
               (Expr_const (Const_int 1))))
             ]))
       ))|}]
;;

let%expect_test "func call with multiple complex arguments" =
  pp pp_expr parse_expr "three(abc, 2 + 3, fac(25))";
  [%expect
    {|
    (Expr_call
       ((Expr_ident "three"),
        [(Expr_ident "abc");
          (Expr_bin_oper (Bin_sum, (Expr_const (Const_int 2)),
             (Expr_const (Const_int 3))));
          (Expr_call ((Expr_ident "fac"), [(Expr_const (Const_int 25))]))]))|}]
;;

let%expect_test "fac_piece2 test" =
  pp pp_expr parse_expr "n <= 1";
  [%expect
    {|
    (Expr_bin_oper (Bin_less_equal, (Expr_ident "n"), (Expr_const (Const_int 1))
       ))|}]
;;

let%expect_test "unary_min test" =
  pp pp_expr parse_expr "-n + 2 + -1";
  [%expect
    {|
    (Expr_bin_oper (Bin_sum,
       (Expr_bin_oper (Bin_sum, (Expr_un_oper (Unary_minus, (Expr_ident "n"))),
          (Expr_const (Const_int 2)))),
       (Expr_un_oper (Unary_minus, (Expr_const (Const_int 1))))))|}]
;;
