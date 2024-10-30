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

let parse_chan_receive =
  lift (fun idt -> Expr_chan_recieve idt) (token "<-" *> parse_ident)
;;

let parse_const_int = parse_int >>| fun num -> Const_int num

let parse_const_string =
  let* _ = char '"' in
  let* string = take_till (Char.equal '"') in
  let* _ = char '"' in
  return (Const_string string)
;;

let parse_idents_with_types =
  let* args_lists =
    sep_by_comma1
      (let* idents = sep_by_comma1 parse_ident in
       let* t = ws_line *> parse_type in
       return (List.map ~f:(fun id -> id, t) idents))
  in
  return (List.concat args_lists)
;;

let parse_func_args = parens parse_idents_with_types <|> (parens ws >>| fun _ -> [])

let parse_func_return_values =
  choice
    [ (parens parse_idents_with_types >>| fun returns -> Some (Ident_and_types returns))
    ; (parens (sep_by_comma1 parse_type) >>| fun types -> Some (Only_types types))
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

let parse_const_func pblock =
  string "func" *> ws *> parse_func_args_returns_and_body pblock
  >>| fun anon_func -> Const_func anon_func
;;

let rec default_init = function
  | Type_int -> Expr_const (Const_int 0)
  | Type_string -> Expr_const (Const_string "")
  | Type_bool -> Expr_const (Const_bool false)
  | Type_chan _ | Type_func _ -> Expr_const Const_nil
  | Type_array (type', size) ->
    Expr_const (Const_array (type', List.init size ~f:(fun _ -> default_init type')))
;;

let parse_const_array pexpr =
  let add_similar_elements lst element count =
    let repeated_elements = List.init count ~f:(fun _ -> element) in
    lst @ repeated_elements
  in
  let array_type_fix size type' lst =
    match type' with
    | Type_int ->
      add_similar_elements lst (Expr_const (Const_int 0)) (size - List.length lst)
    | Type_string ->
      add_similar_elements lst (Expr_const (Const_string "")) (size - List.length lst)
    | Type_bool ->
      add_similar_elements lst (Expr_const (Const_bool false)) (size - List.length lst)
    | _ -> lst
  in
  lift3
    (fun size type' list_exprs ->
      Const_array (type', array_type_fix size type' list_exprs))
    (square_brackets parse_int)
    (ws *> parse_type)
    (curly_braces (sep_by_comma pexpr <|> list []))
;;

let parse_const pexpr pblock =
  choice
    [ parse_const_int
    ; parse_const_string
    ; parse_const_array pexpr
    ; parse_const_func pblock
    ]
  >>| fun const -> Expr_const const
;;

let parse_ident = parse_ident_not_blank >>| fun ident -> Expr_ident ident

let parse_expr_func_call pexpr func_expr =
  let* func = func_expr <* ws_line in
  let* args = parens (sep_by_comma pexpr) in
  return (Expr_call (func, args))
;;

let parse_index pexpr array_expr =
  let* array = array_expr <* ws_line in
  let* index = square_brackets pexpr in
  return (Expr_index (array, index))
;;

let parse_atomic_expr pexpr pblock =
  choice [ parse_ident; parse_const pexpr pblock; parse_chan_receive ]
;;

let parse_expr pblock =
  fix (fun pexpr ->
    let arg = parens pexpr <|> parse_atomic_expr pexpr pblock in
    let arg = parse_expr_func_call pexpr arg <|> parse_index pexpr arg <|> arg in
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
