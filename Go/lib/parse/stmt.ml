(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Angstrom
open Common
open Expr

let rec combine_lists l1 l2 =
  match l1, l2 with
  | [], [] -> []
  | x :: xs, y :: ys -> (x, y) :: combine_lists xs ys
  | _, _ -> assert false (* bad, mb [] instead *)
;;

let parse_lvalues = sep_by1 (ws_line *> char ',' *> ws) parse_ident
let parse_rvalues = sep_by1 (ws_line *> char ',' *> ws) parse_expr

let parse_long_var_decl =
  let* _ = string "var" *> ws in
  let* lvalues = parse_lvalues <* ws_line in
  let* vars_type = parse_type >>| (fun t -> Some t) <|> return None in
  let* with_init = ws_line *> char '=' *> ws *> return true <|> return false in
  if not with_init
  then (
    match vars_type with
    | Some t -> return (Long_decl_no_init (t, lvalues))
    | None -> fail "Long variable declaration without initializers should have type")
  else
    let* rvalues = parse_rvalues in
    if List.length lvalues = 0
    then fail "No identifiers in long variable declaration"
    else (
      match rvalues, List.length lvalues = List.length rvalues with
      | _ :: _, true ->
        return (Long_decl_mult_init (vars_type, combine_lists lvalues rvalues))
      | _ :: _, false ->
        if List.length rvalues = 1
        then (
          match List.nth rvalues 0 with
          | Some (Expr_call _ as expr) ->
            return (Long_decl_one_init (vars_type, lvalues, expr))
          | Some _ | None ->
            fail
              "Initializer has to ba a function call in variable declarations with \
               multiple identifiers and one initializer")
        else
          fail
            "Number of lvalues and rvalues in variable declarations should be the same \
             or rvalue should be a function that returns multiple values"
      | [], _ -> assert false)
;;

let parse_short_var_decl =
  let* lvalues = parse_lvalues in
  let* _ = ws_line *> string ":=" *> ws in
  let* rvalues = parse_rvalues in
  if List.length lvalues = 0 || List.length rvalues = 0
  then fail "No identifiers or initializers in short vaiable declarations"
  else if List.length lvalues != List.length rvalues
  then
    if List.length rvalues = 1
    then (
      match List.nth rvalues 0 with
      | Some (Expr_call _ as expr) ->
        return (Stmt_short_var_decl (Short_decl_one_init (lvalues, expr)))
      | Some _ | None ->
        fail
          "Initializer has to ba a function call in variavle declarations with multiple \
           identifiers and one initializer")
    else
      fail
        "Number of lvalues and rvalues should be the same or rvalue should be a function \
         that returns multiple values"
  else return (Stmt_short_var_decl (Short_decl_mult_init (combine_lists lvalues rvalues)))
;;

let parse_assign =
  let* lvalues = parse_lvalues in
  let* _ = ws_line *> char '=' *> ws in
  let* rvalues = parse_rvalues in
  if List.length lvalues = 0 || List.length rvalues = 0
  then fail "No identifiers or initializers in assignment"
  else if List.length lvalues != List.length rvalues
  then
    if List.length rvalues == 1
    then (
      match List.nth rvalues 0 with
      | Some (Expr_call _ as expr) ->
        return (Stmt_assign (Assign_one_expr (lvalues, expr)))
      | Some _ | None ->
        fail
          "Initializer has to ba a function call in assignments with multiple \
           identifiers and one initializer")
    else
      fail
        "Number of identifiers and initializers is not equal in assignment or \
         initializer is not a single function with multiple returns"
  else return (Stmt_assign (Assign_mult_expr (combine_lists lvalues rvalues)))
;;

let parse_incr =
  parse_ident_not_blank <* ws_line <* string "++" >>| fun id -> Stmt_incr id
;;

let parse_decr =
  parse_ident_not_blank <* ws_line <* string "--" >>| fun id -> Stmt_decr id
;;

let parse_stmt_call = parse_func_call parse_expr >>| fun call -> Stmt_call call

(* let parse_chan_send = return ()
   let parse_chan_receive = return () *)

let parse_defer =
  string "defer" *> ws *> parse_func_call parse_expr >>| fun call -> Stmt_defer call
;;

let parse_go =
  string "go" *> ws *> parse_func_call parse_expr >>| fun call -> Stmt_go call
;;

let parse_chan_send =
  lift2
    (fun idt expr -> Stmt_chan_send (idt, expr))
    (ws *> parse_ident)
    (token "<-" *> parse_expr)
;;

let parse_break = string "break" *> return Stmt_break
let parse_continue = string "continue" *> return Stmt_continue

let parse_return =
  string "return" *> ws_line *> sep_by (ws_line *> char ',' *> ws) parse_expr
  >>| fun expr_list -> Stmt_return expr_list
;;

let is_valid_init = function
  | Some (Stmt_short_var_decl _)
  | Some (Stmt_assign _)
  | Some (Stmt_incr _)
  | Some (Stmt_decr _)
  | Some (Stmt_call _)
  | None -> true
  | _ -> false
;;

let parse_if pstmt pblock =
  let* _ = string "if" *> ws in
  let* init = pstmt >>| (fun init -> Some init) <|> return None in
  if not (is_valid_init init)
  then fail "Incorrect statement in if initialization"
  else
    let* _ = parse_stmt_sep <|> return () in
    let* cond = ws *> parse_expr in
    let* if_body = ws_line *> pblock <* ws_line in
    let* else_body =
      let* else_body_exists = string "else" *> ws *> return true <|> return false in
      if else_body_exists
      then
        let* else_body = pstmt in
        match else_body with
        | Stmt_if _ | Stmt_block _ -> return (Some else_body)
        | _ -> fail "Only block or if statement can be used after else"
      else return None
      (* *> (parse_if pstmt pblock
         >>| (fun if_stmt -> Some if_stmt)
         <|> (pblock >>| fun block -> Some (Stmt_block block))
         <|> fail "Only block or if statement can be ised after else")
         <|> return None *)
    in
    return (Stmt_if { init; cond; if_body; else_body })
;;

let parse_for pstmt pblock =
  let parse_default_for =
    let* init = pstmt >>| (fun stmt -> Some stmt) <|> return None in
    let ok_init = if is_valid_init init then true else false in
    let* _ = parse_stmt_sep in
    let* cond = parse_expr >>| (fun expr -> Some expr) <|> return None in
    let* _ = parse_stmt_sep in
    let* post =
      let* next_char = peek_char_fail in
      match next_char with
      | '{' -> return None
      | _ -> pstmt >>| fun stmt -> Some stmt
    in
    let ok_post =
      match post with
      | Some (Stmt_short_var_decl _)
      | Some (Stmt_assign _)
      | Some (Stmt_incr _)
      | Some (Stmt_decr _)
      | Some (Stmt_call _)
      | None -> true
      | _ -> false
    in
    if ok_init && ok_post
    then return (init, cond, post)
    else fail "Incorrect statement in for initialization or post statemnent"
  in
  let parse_for_only_cond =
    let* next_char = peek_char_fail in
    match next_char with
    | '{' -> return (None, None, None)
    | _ -> parse_expr >>| fun cond -> None, Some cond, None
  in
  let parse_for_range_n =
    let* _ = string "range" *> ws in
    let* expr = parse_expr in
    return
      ( Some (Stmt_short_var_decl (Short_decl_mult_init [ "i", Expr_const (Const_int 0) ]))
      , Some (Expr_bin_oper (Bin_less, Expr_ident "i", expr))
      , Some (Stmt_incr "i") )
  in
  let* _ = string "for" *> ws in
  let* init, cond, post =
    choice [ parse_default_for; parse_for_only_cond; parse_for_range_n ]
  in
  let* body = ws_line *> pblock in
  return (Stmt_for { init; cond; post; body })
;;

(* можно парсить [for range 1000] как [for i := 0; i < 1000; i++]
   let parse_range = return () *)

let parse_stmt pblock =
  fix (fun pstmt ->
    choice
      [ (parse_long_var_decl >>| fun decl -> Stmt_long_var_decl decl)
      ; parse_short_var_decl
      ; parse_incr
      ; parse_decr
      ; parse_if pstmt pblock
      ; parse_chan_send
      ; parse_break
      ; parse_continue
      ; parse_return
      ; parse_stmt_call
      ; parse_assign
      ; parse_defer
      ; parse_go
      ; (pblock >>| fun block -> Stmt_block block)
      ; parse_for pstmt pblock
        (*
           ; parse_range
           ; parse_chan_send
           ; parse_chan_receive *)
      ]
      ~failure_msg:"Incorrect statement")
;;

let parse_block : block t =
  fix (fun pblock ->
    char '{'
    *> skip_many (ws *> parse_stmt_sep *> ws)
    *> ws
    *> sep_by (many1 parse_stmt_sep) (parse_stmt pblock)
    <* skip_many (ws *> parse_stmt_sep *> ws)
    <* ws
    <* char '}')
;;

(**************************************** Tests ****************************************)

let pstmt = parse_stmt parse_block (* for tests *)

let%expect_test "break stmt" =
  pp pp_stmt pstmt {|break|};
  [%expect {| Stmt_break |}]
;;

let%expect_test "continue stmt" =
  pp pp_stmt pstmt {|continue|};
  [%expect {| Stmt_continue |}]
;;

let%expect_test "incr stmt" =
  pp pp_stmt pstmt {|a++|};
  [%expect {| (Stmt_incr "a") |}]
;;

let%expect_test "incr stmt with ws_line" =
  pp pp_stmt pstmt {|a    /* some comment */   ++|};
  [%expect {| (Stmt_incr "a") |}]
;;

let%expect_test "incr stmt with blank ident" =
  pp pp_stmt pstmt {|_++|};
  [%expect {| : Incorrect statement |}]
;;

let%expect_test "decr stmt" =
  pp pp_stmt pstmt {|a--|};
  [%expect {| (Stmt_decr "a") |}]
;;

let%expect_test "decr stmt with ws_line" =
  pp pp_stmt pstmt {|a    /* some comment */   --|};
  [%expect {| (Stmt_decr "a") |}]
;;

let%expect_test "decr stmt with blank ident" =
  pp pp_stmt pstmt {|_--|};
  [%expect {| : Incorrect statement |}]
;;

let%expect_test "return without anything" =
  pp pp_stmt pstmt {|return|};
  [%expect {| (Stmt_return []) |}]
;;

let%expect_test "return with one expr" =
  pp pp_stmt pstmt {|return|};
  [%expect {| (Stmt_return []) |}]
;;

let%expect_test "return with one expr" =
  pp pp_stmt pstmt {|return 5|};
  [%expect {| (Stmt_return [(Expr_const (Const_int 5))]) |}]
;;

let%expect_test "return with multiple exprs and ws" =
  pp
    pp_stmt
    pstmt
    {|return 3    ,   
             a  ,  // some comment 
             true /* RARAVARV */    ,  nil|};
  [%expect
    {|
    (Stmt_return
       [(Expr_const (Const_int 3)); (Expr_ident "a"); (Expr_ident "true");
         (Expr_ident "nil")]) |}]
;;

(* не работает из-за экспрешенов *)
let%expect_test "return with multiple complex exprs" =
  pp pp_stmt pstmt {|return -5 * _r + 8, !a && (b || c)|};
  [%expect
    {|
    (Stmt_return
       [(Expr_bin_oper (Bin_sum,
           (Expr_bin_oper (Bin_multiply,
              (Expr_un_oper (Unary_minus, (Expr_const (Const_int 5)))),
              (Expr_ident "_r"))),
           (Expr_const (Const_int 8))));
         (Expr_bin_oper (Bin_and, (Expr_un_oper (Unary_not, (Expr_ident "a"))),
            (Expr_bin_oper (Bin_or, (Expr_ident "b"), (Expr_ident "c")))))
         ]) |}]
;;

let%expect_test "stmt func call with one simple arg" =
  pp pp_stmt pstmt {|my_func(5)|};
  [%expect {| (Stmt_call ((Expr_ident "my_func"), [(Expr_const (Const_int 5))])) |}]
;;

let%expect_test "stmt func callmultiple args" =
  pp pp_stmt pstmt {|my_func(5, a, nil)|};
  [%expect
    {|
    (Stmt_call
       ((Expr_ident "my_func"),
        [(Expr_const (Const_int 5)); (Expr_ident "a"); (Expr_ident "nil")])) |}]
;;

let%expect_test "stmt func call with complex expressions and comments" =
  pp pp_stmt pstmt {|fac(   fac(2 + 2), 
  34 * 75,
  // aovnervo 
  !a)|};
  [%expect
    {|
    (Stmt_call
       ((Expr_ident "fac"),
        [(Expr_call
            ((Expr_ident "fac"),
             [(Expr_bin_oper (Bin_sum, (Expr_const (Const_int 2)),
                 (Expr_const (Const_int 2))))
               ]));
          (Expr_bin_oper (Bin_multiply, (Expr_const (Const_int 34)),
             (Expr_const (Const_int 75))));
          (Expr_un_oper (Unary_not, (Expr_ident "a")))])) |}]
;;

let%expect_test "stmt assign one lvalue, one rvalue" =
  pp pp_stmt pstmt {|a = 5|};
  [%expect {| (Stmt_assign (Assign_mult_expr [("a", (Expr_const (Const_int 5)))])) |}]
;;

let%expect_test "stmt assign with mult equal number of lvalues and rvalues and ws" =
  pp
    pp_stmt
    pstmt
    {|a, 
  b , // comment
  c = 
  
  5, /* comment////// */true,
   "hello"|};
  [%expect
    {|
    (Stmt_assign
       (Assign_mult_expr
          [("a", (Expr_const (Const_int 5))); ("b", (Expr_ident "true"));
            ("c", (Expr_const (Const_string "hello")))])) |}]
;;

let%expect_test "stmt assign mult lvalues and one rvalue that is a func call" =
  pp pp_stmt pstmt {|a, b ,c = get_three()|};
  [%expect
    {|
    (Stmt_assign
       (Assign_one_expr (["a"; "b"; "c"],
          (Expr_call ((Expr_ident "get_three"), []))))) |}]
;;

let%expect_test "stmt assign mult lvalues and one rvalue that is not a func call" =
  pp pp_stmt pstmt {|a, b ,c = abc|};
  [%expect {| : Incorrect statement |}]
;;

let%expect_test "stmt assign mult unequal lvalues and rvalues" =
  pp pp_stmt pstmt {|a, b ,c = 2, 3, 4, 5 , 6|};
  [%expect {| : Incorrect statement |}]
;;

let%expect_test "stmt long single var decl without init" =
  pp pp_stmt pstmt {|var a int|};
  [%expect {|
    (Stmt_long_var_decl (Long_decl_no_init (Type_int, ["a"]))) |}]
;;

let%expect_test "stmt long single var decl no type" =
  pp pp_stmt pstmt {|var a = 5|};
  [%expect
    {|
    (Stmt_long_var_decl
       (Long_decl_mult_init (None, [("a", (Expr_const (Const_int 5)))]))) |}]
;;

let%expect_test "stmt long mult var decl no type" =
  pp pp_stmt pstmt {|var a, b, c = 5, nil, "hi"|};
  [%expect
    {|
    (Stmt_long_var_decl
       (Long_decl_mult_init (None,
          [("a", (Expr_const (Const_int 5))); ("b", (Expr_ident "nil"));
            ("c", (Expr_const (Const_string "hi")))]
          ))) |}]
;;

(* не работает из-за экспрешенов *)
let%expect_test "stmt long single var decl with type" =
  pp pp_stmt pstmt {|var a func() = func() {}|};
  [%expect {| : Incorrect statement |}]
;;

let%expect_test "stmt long mult var decl with type" =
  pp pp_stmt pstmt {|var a, b int = 2, 3|};
  [%expect
    {|
    (Stmt_long_var_decl
       (Long_decl_mult_init ((Some Type_int),
          [("a", (Expr_const (Const_int 2))); ("b", (Expr_const (Const_int 3)))]
          ))) |}]
;;

(* нет константных массивов *)
let%expect_test "stmt long mult var decl with type" =
  pp pp_stmt pstmt {|var a, b, c [2]int = [2]int{1, 2}, [2]int{}, [2]int{10, 20}|};
  [%expect
    {|
    (Stmt_long_var_decl
       (Long_decl_mult_init ((Some (Type_array (Type_int, 2))),
          [("a",
            (Expr_array (Type_int,
               [(Expr_const (Const_int 1)); (Expr_const (Const_int 2))])));
            ("b",
             (Expr_array (Type_int,
                [(Expr_const (Const_int 0)); (Expr_const (Const_int 0))])));
            ("c",
             (Expr_array (Type_int,
                [(Expr_const (Const_int 10)); (Expr_const (Const_int 20))])))
            ]
          ))) |}]
;;

let%expect_test "stmt long single var decl with type" =
  pp pp_stmt pstmt {|var a, b, c = 5, nil, "hi"|};
  [%expect
    {|
    (Stmt_long_var_decl
       (Long_decl_mult_init (None,
          [("a", (Expr_const (Const_int 5))); ("b", (Expr_ident "nil"));
            ("c", (Expr_const (Const_string "hi")))]
          ))) |}]
;;

let%expect_test "stmt long var decl mult lvalues and one rvalue that is a func call" =
  pp pp_stmt pstmt {|var a, b, c = get_three(1, 2, 3)|};
  [%expect
    {|
    (Stmt_long_var_decl
       (Long_decl_one_init (None, ["a"; "b"; "c"],
          (Expr_call
             ((Expr_ident "get_three"),
              [(Expr_const (Const_int 1)); (Expr_const (Const_int 2));
                (Expr_const (Const_int 3))]))
          ))) |}]
;;

let%expect_test "stmt long var decl mult lvalues and one rvalue that is not a func call" =
  pp pp_stmt pstmt {|var a, b, c = true|};
  [%expect {|
    : Incorrect statement |}]
;;

let%expect_test "stmt long var decl unequal lvalues and rvalues" =
  pp pp_stmt pstmt {|var a, b, c = 1, 2, 3, 4|};
  [%expect {|
    : Incorrect statement |}]
;;

let%expect_test "stmt short single var decl" =
  pp pp_stmt pstmt {|a := 7|};
  [%expect
    {|
    (Stmt_short_var_decl
       (Short_decl_mult_init [("a", (Expr_const (Const_int 7)))])) |}]
;;

let%expect_test "stmt short mult var decl" =
  pp pp_stmt pstmt {|a, b, c := true, 567, "string"|};
  [%expect
    {|
    (Stmt_short_var_decl
       (Short_decl_mult_init
          [("a", (Expr_ident "true")); ("b", (Expr_const (Const_int 567)));
            ("c", (Expr_const (Const_string "string")))])) |}]
;;

let%expect_test "stmt short var decl mult lvalues and one rvalue that is a func call" =
  pp pp_stmt pstmt {|a, b, c := three(abc, 2 + 3, fac(25))|};
  [%expect
    {|
    (Stmt_short_var_decl
       (Short_decl_one_init (["a"; "b"; "c"],
          (Expr_call
             ((Expr_ident "three"),
              [(Expr_ident "abc");
                (Expr_bin_oper (Bin_sum, (Expr_const (Const_int 2)),
                   (Expr_const (Const_int 3))));
                (Expr_call ((Expr_ident "fac"), [(Expr_const (Const_int 25))]))]))
          ))) |}]
;;

let%expect_test "stmt short var decl mult lvalues and one rvalue that is not a func call" =
  pp pp_stmt pstmt {|a, b, c := abcdefg"|};
  [%expect {|
    : Incorrect statement |}]
;;

let%expect_test "stmt short var decl unequal lvalues and rvalues" =
  pp pp_stmt pstmt {|a, b, c := 1, 2, 3, 4|};
  [%expect {|
    : Incorrect statement |}]
;;

let%expect_test "stmt defer with func" =
  pp pp_stmt pstmt {|defer 
                      call(abc)|};
  [%expect {|
    (Stmt_defer ((Expr_ident "call"), [(Expr_ident "abc")])) |}]
;;

let%expect_test "stmt defer with expr that is not a func" =
  pp pp_stmt pstmt {|defer 2 + 2 * 5|};
  [%expect {|
    : Incorrect statement |}]
;;

let%expect_test "stmt go with func" =
  pp pp_stmt pstmt {|go 
                      call(abc)|};
  [%expect {|
    (Stmt_go ((Expr_ident "call"), [(Expr_ident "abc")])) |}]
;;

let%expect_test "stmt go with expr that is not a func" =
  pp pp_stmt pstmt {|go 2 + 2 * 5|};
  [%expect {|
    : Incorrect statement |}]
;;

let%expect_test "stmt empty block" =
  pp pp_stmt pstmt {|{}|};
  [%expect {|
    (Stmt_block []) |}]
;;

let%expect_test "stmt block of one stmt" =
  pp pp_stmt pstmt {|{ a := 5 }|};
  [%expect
    {|
    (Stmt_block
       [(Stmt_short_var_decl
           (Short_decl_mult_init [("a", (Expr_const (Const_int 5)))]))
         ]) |}]
;;

let%expect_test "stmt block of mult stmts, separated by semicolon" =
  pp pp_stmt pstmt {|{ a := 5; a++; println(a) }|};
  [%expect
    {|
    (Stmt_block
       [(Stmt_short_var_decl
           (Short_decl_mult_init [("a", (Expr_const (Const_int 5)))]));
         (Stmt_incr "a");
         (Stmt_call ((Expr_ident "println"), [(Expr_ident "a")]))]) |}]
;;

let%expect_test "stmt block of mult stmts, separated by newlines" =
  pp
    pp_stmt
    pstmt
    {|{ var hi string = "hi"
    // string that says hi
      go get_int(hi)}|};
  [%expect
    {|
    (Stmt_block
       [(Stmt_long_var_decl
           (Long_decl_mult_init ((Some Type_string),
              [("hi", (Expr_const (Const_string "hi")))])));
         (Stmt_go ((Expr_ident "get_int"), [(Expr_ident "hi")]))]) |}]
;;

let%expect_test "chan send sttmt" =
  pp pp_stmt pstmt {|c <- sum + 1|};
  [%expect
    {|
    (Stmt_chan_send ("c",
       (Expr_bin_oper (Bin_sum, (Expr_ident "sum"), (Expr_const (Const_int 1))))
       )) |}]
;;

let%expect_test "stmt simple if" =
  pp pp_stmt pstmt {|if true {}|};
  [%expect
    {|
    Stmt_if {init = None; cond = (Expr_ident "true"); if_body = [];
      else_body = None} |}]
;;

let%expect_test "stmt if with init" =
  pp pp_stmt pstmt {|if k := 0; k == test {}|};
  [%expect
    {|
    Stmt_if {
      init =
      (Some (Stmt_short_var_decl
               (Short_decl_mult_init [("k", (Expr_const (Const_int 0)))])));
      cond = (Expr_bin_oper (Bin_equal, (Expr_ident "k"), (Expr_ident "test")));
      if_body = []; else_body = None} |}]
;;

let%expect_test "stmt if with empty init" =
  pp pp_stmt pstmt {|if ; call() {}|};
  [%expect
    {|
    Stmt_if {init = None; cond = (Expr_call ((Expr_ident "call"), []));
      if_body = []; else_body = None} |}]
;;

let%expect_test "stmt if with wrong init" =
  pp pp_stmt pstmt {|if var a = 5; cond {}|};
  [%expect {|
    : Incorrect statement |}]
;;

let%expect_test "stmt if with else that is a block" =
  pp pp_stmt pstmt {|if cond {} else {}|};
  [%expect {|
    Stmt_if {init = None; cond = (Expr_ident "cond"); if_body = [];
      else_body = (Some (Stmt_block []))} |}]
;;

let%expect_test "stmt if with else that is another if" =
  pp pp_stmt pstmt {|if cond {} else if cond2 {}|};
  [%expect {|
    Stmt_if {init = None; cond = (Expr_ident "cond"); if_body = [];
      else_body =
      (Some Stmt_if {init = None; cond = (Expr_ident "cond2"); if_body = [];
              else_body = None})} |}]
;;

let%expect_test "stmt if with wrong else" =
  pp pp_stmt pstmt {|if cond {} else do_smth()|};
  [%expect {|
    : Incorrect statement |}]
;;

let%expect_test "stmt empty for" =
  pp pp_stmt pstmt {|for {}|};
  [%expect {|
    Stmt_for {init = None; cond = None; post = None; body = []} |}]
;;

let%expect_test "stmt for with only conition" =
  pp pp_stmt pstmt {|for a > 0 {}|};
  [%expect
    {|
    Stmt_for {init = None;
      cond =
      (Some (Expr_bin_oper (Bin_greater, (Expr_ident "a"),
               (Expr_const (Const_int 0)))));
      post = None; body = []} |}]
;;

let%expect_test "stmt empty for with semicolons" =
  pp pp_stmt pstmt {|for ;; {}|};
  [%expect {|
    Stmt_for {init = None; cond = None; post = None; body = []} |}]
;;

let%expect_test "stmt simple for" =
  pp pp_stmt pstmt {|for i := 0; i < 10; i++ {}|};
  [%expect
    {|
    Stmt_for {
      init =
      (Some (Stmt_short_var_decl
               (Short_decl_mult_init [("i", (Expr_const (Const_int 0)))])));
      cond =
      (Some (Expr_bin_oper (Bin_less, (Expr_ident "i"),
               (Expr_const (Const_int 10)))));
      post = (Some (Stmt_incr "i")); body = []} |}]
;;

let%expect_test "stmt for with range and number" =
  pp pp_stmt pstmt {|for range 10 {}|};
  [%expect
    {|
    Stmt_for {
      init =
      (Some (Stmt_short_var_decl
               (Short_decl_mult_init [("i", (Expr_const (Const_int 0)))])));
      cond =
      (Some (Expr_bin_oper (Bin_less, (Expr_ident "i"),
               (Expr_const (Const_int 10)))));
      post = (Some (Stmt_incr "i")); body = []} |}]
;;
