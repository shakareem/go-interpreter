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
  let parse_ident_not_blank =
    let* ident = parse_ident in
    match ident with
    | "_" -> fail "Blank identifier is a write-only value"
    | _ -> return ident
  in
  parse_ident_not_blank
  >>| (fun id -> Expr_ident id)
  <|> (parse_const >>| fun const -> Expr_const const)
;;

(* нужно переиспользовать в стейтментах *)
let parse_func_call pexpr =
  let* func_name = ws *> parse_ident <* ws_line in
  let* args = parens (sep_by (ws_line *> char ',' *> ws) pexpr) in
  return (Expr_call (Expr_ident func_name, args))
;;

let parse_expr =
  fix (fun expr ->
    let arg = parse_func_call expr <|> parse_simple_expr in
    let arg = penot arg <|> arg in
    let arg = peusb arg <|> arg in
    let arg = chainl1 arg (pemul <|> pemod <|> pediv) in
    let arg = chainl1 arg (pesum <|> pesub) in
    let arg = chainl1 arg (pegre <|> pelse <|> pegrt <|> pelss <|> peeql <|> penql) in
    let arg = fix (fun _ -> arg) <|> arg in
    arg)
;;

(* only (a, b, c int) args supported, TODO: (a string, b int) *)
let parse_func_args =
  parens
    (let* idents = sep_by (ws_line *> char ',' *> ws) parse_ident in
     let* t = ws_line *> parse_type in
     return (List.map ~f:(fun id -> id, t) idents))
;;

(* TODO: (a int, b string) *)
let parse_func_return_values =
  parse_type >>| (fun t -> Only_types [ t ]) <|> return (Only_types [])
;;

let parse_anon_func pblock =
  let* args = parse_func_args <* ws_line in
  let* returns =
    parse_func_return_values
    >>| (fun returns ->
          match returns with
          | Only_types (_ :: _) | Ident_and_types _ -> Some returns
          | Only_types [] -> None)
    <* ws_line
  in
  let* body = pblock in
  return { args; returns; body }
;;

let parse_expr_anon_func parse_block = string "func" *> ws *> parse_anon_func parse_block

(******************************* Tests ********************************)

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
