(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Parse
open Pprinter.Printer
open Pp

(********** break, continue, go, defer and channel send **********)

let%expect_test "break stmt" =
  pp print_stmt parse_stmt {|break|};
  [%expect {| break |}]
;;

let%expect_test "continue stmt" =
  pp print_stmt parse_stmt {|continue|};
  [%expect {| continue |}]
;;

let%expect_test "stmt defer with func" =
  pp print_stmt parse_stmt {|defer 
                      call(abc)|};
  [%expect {|
    defer call(abc) |}]
;;

let%expect_test "stmt defer with expr that is not a func" =
  pp print_stmt parse_stmt {|defer 2 + 2 * 5|};
  [%expect {|
    : Incorrect statement |}]
;;

let%expect_test "stmt go with func" =
  pp print_stmt parse_stmt {|go 
                      call(abc)|};
  [%expect {|
    go call(abc) |}]
;;

let%expect_test "stmt go with expr that is not a func" =
  pp print_stmt parse_stmt {|go 2 + 2 * 5|};
  [%expect {|
    : Incorrect statement |}]
;;

let%expect_test "stmt chan send" =
  pp print_stmt parse_stmt {|c <- sum + 1|};
  [%expect {|
    c <- sum + 1 |}]
;;

(********** incr and decr **********)

let%expect_test "incr stmt" =
  pp print_stmt parse_stmt {|a++|};
  [%expect {| a++ |}]
;;

let%expect_test "incr stmt with ws_line" =
  pp print_stmt parse_stmt {|a    /* some comment */   ++|};
  [%expect {| a++ |}]
;;

let%expect_test "incr stmt with blank ident" =
  pp print_stmt parse_stmt {|_++|};
  [%expect {| _++ |}]
;;

let%expect_test "decr stmt" =
  pp print_stmt parse_stmt {|a--|};
  [%expect {| a-- |}]
;;

let%expect_test "decr stmt with ws_line" =
  pp print_stmt parse_stmt {|a    /* some comment */   --|};
  [%expect {| a-- |}]
;;

let%expect_test "decr stmt with blank ident" =
  pp print_stmt parse_stmt {|_--|};
  [%expect {| _-- |}]
;;

(********** return **********)

let%expect_test "return without anything" =
  pp print_stmt parse_stmt {|return|};
  [%expect {| return |}]
;;

let%expect_test "return with one expr" =
  pp print_stmt parse_stmt {|return 5|};
  [%expect {| return 5 |}]
;;

let%expect_test "return with multiple exprs and ws" =
  pp
    print_stmt
    parse_stmt
    {|return 3    ,   
             a  ,  // some comment 
             true /* RARAVARV */    ,  nil|};
  [%expect {|
    return 3, a, true, nil |}]
;;

let%expect_test "return with multiple complex exprs" =
  pp print_stmt parse_stmt {|return -5 * _r + 8, !a && (b || c)|};
  [%expect {|
    return -5 * _r + 8, !a && (b || c) |}]
;;

(********** func call **********)

let%expect_test "stmt func call with one simple arg" =
  pp print_stmt parse_stmt {|my_func(5)|};
  [%expect {| my_func(5) |}]
;;

let%expect_test "stmt func callmultiple args" =
  pp print_stmt parse_stmt {|my_func(5, a, nil)|};
  [%expect {|
    my_func(5, a, nil) |}]
;;

let%expect_test "stmt func call with complex expressions and comments" =
  pp print_stmt parse_stmt {|fac(   fac(2 + 2), 
  34 * 75,
  // aovnervo 
  !a)|};
  [%expect {|
    fac(fac(2 + 2), 34 * 75, !a) |}]
;;

(********** assign **********)

let%expect_test "stmt assign one lvalue, one rvalue" =
  pp print_stmt parse_stmt {|a = 5|};
  [%expect {|
    a = 5 |}]
;;

let%expect_test "stmt assign one lvalue that is an array index, one rvalue" =
  pp print_stmt parse_stmt {|a[i][2 + 3] = 5|};
  [%expect {|
    a[i][2 + 3] = 5 |}]
;;

let%expect_test "stmt assign with mult equal number of lvalues and rvalues and ws" =
  pp
    print_stmt
    parse_stmt
    {|a, 
  b , // comment
  c[get_index()] = 
  
  5, /* comment////// */true,
   "hello"|};
  [%expect {|
    a, b, c[get_index()] = 5, true, "hello" |}]
;;

let%expect_test "stmt assign mult lvalues and one rvalue that is a func call" =
  pp print_stmt parse_stmt {|a, b[0] ,c = get_three()|};
  [%expect {|
    a, b[0], c = get_three() |}]
;;

let%expect_test "stmt assign mult lvalues and one rvalue that is not a func call" =
  pp print_stmt parse_stmt {|a, b ,c = abc|};
  [%expect {| : Incorrect statement |}]
;;

let%expect_test "stmt assign mult unequal lvalues and rvalues" =
  pp print_stmt parse_stmt {|a, b ,c = 2, 3, 4, 5 , 6|};
  [%expect {| : Incorrect statement |}]
;;

(********** long var decl **********)

let%expect_test "stmt long single var decl without init" =
  pp print_stmt parse_stmt {|var a string|};
  [%expect {|
    var a string |}]
;;

let%expect_test "stmt long single var decl without init with mult array type" =
  pp print_stmt parse_stmt {|var a [2][3][1]bool|};
  [%expect {|
    var a [2][3][1]bool |}]
;;

let%expect_test "stmt long single var decl no type" =
  pp print_stmt parse_stmt {|var a = 5|};
  [%expect {|
    var a = 5 |}]
;;

let%expect_test "stmt long mult var decl no type" =
  pp print_stmt parse_stmt {|var a, b, c = 5, nil, "hi"|};
  [%expect {|
    var a, b, c = 5, nil, "hi" |}]
;;

let%expect_test "stmt long single var decl with type" =
  pp print_stmt parse_stmt {|var a func() = func() {}|};
  [%expect {|
    var a func() = func() {} |}]
;;

let%expect_test "stmt long mult var decl with type" =
  pp print_stmt parse_stmt {|var a, b int = 2, 3|};
  [%expect {|
    var a, b int = 2, 3 |}]
;;

let%expect_test "stmt long mult var decl with type" =
  pp print_stmt parse_stmt {|var a, b, c [2]int = [2]int{1, 2}, [2]int{}, [2]int{10, 20}|};
  [%expect {|
    var a, b, c [2]int = [2]int{1, 2}, [2]int{}, [2]int{10, 20} |}]
;;

let%expect_test "stmt long mult var decl without type" =
  pp print_stmt parse_stmt {|var a, b, c = 5, nil, "hi"|};
  [%expect {|
    var a, b, c = 5, nil, "hi" |}]
;;

let%expect_test "stmt long var decl mult lvalues and one rvalue that is a func call" =
  pp print_stmt parse_stmt {|var a, b, c = get_three(1, 2, 3)|};
  [%expect {|
    var a, b, c  = get_three(1, 2, 3) |}]
;;

let%expect_test "stmt long var decl mult lvalues and one rvalue that is not a func call" =
  pp print_stmt parse_stmt {|var a, b, c = true|};
  [%expect {|
    : Incorrect statement |}]
;;

let%expect_test "stmt long var decl unequal lvalues and rvalues" =
  pp print_stmt parse_stmt {|var a, b, c = 1, 2, 3, 4|};
  [%expect {|
    : Incorrect statement |}]
;;

(********** short var decl **********)

let%expect_test "stmt short single var decl" =
  pp print_stmt parse_stmt {|a := 7|};
  [%expect {|
    a := 7 |}]
;;

let%expect_test "stmt short mult var decl" =
  pp print_stmt parse_stmt {|a, b, c := true, 567, "string"|};
  [%expect {|
    a, b, c := true, 567, "string" |}]
;;

let%expect_test "stmt short var decl mult lvalues and one rvalue that is a func call" =
  pp print_stmt parse_stmt {|a, b, c := three(abc, 2 + 3, fac(25))|};
  [%expect {|
    a, b, c := three(abc, 2 + 3, fac(25)) |}]
;;

let%expect_test "stmt short var decl mult lvalues and one rvalue that is not a func call" =
  pp print_stmt parse_stmt {|a, b, c := abcdefg"|};
  [%expect {|
    : Incorrect statement |}]
;;

let%expect_test "stmt short var decl unequal lvalues and rvalues" =
  pp print_stmt parse_stmt {|a, b, c := 1, 2, 3, 4|};
  [%expect {|
    : Incorrect statement |}]
;;

(********** block **********)

let%expect_test "stmt empty block" =
  pp print_stmt parse_stmt {|{}|};
  [%expect {|
    {} |}]
;;

let%expect_test "stmt block of one stmt" =
  pp print_stmt parse_stmt {|{ a := 5 }|};
  [%expect {|
    {
        a := 5
    } |}]
;;

let%expect_test "stmt block of mult stmts, separated by semicolon" =
  pp print_stmt parse_stmt {|{ a := 5; a++; println(a) }|};
  [%expect {|
    {
        a := 5
        a++
        println(a)
    } |}]
;;

let%expect_test "stmt block of mult stmts, separated by newlines" =
  pp
    print_stmt
    parse_stmt
    {|{ var hi string = "hi"
    // string that says hi
      go get_int(hi)}|};
  [%expect {|
    {
        var hi string = "hi"
        go get_int(hi)
    } |}]
;;

(********** if **********)

let%expect_test "stmt simple if" =
  pp print_stmt parse_stmt {|if true {}|};
  [%expect {|
    if true {} |}]
;;

let%expect_test "stmt if with init" =
  pp print_stmt parse_stmt {|if k := 0; k == test {}|};
  [%expect {|
    if k := 0; k == test {} |}]
;;

let%expect_test "stmt if with empty init" =
  pp print_stmt parse_stmt {|if ; call() {}|};
  [%expect {|
    if call() {} |}]
;;

let%expect_test "stmt if with wrong init" =
  pp print_stmt parse_stmt {|if var a = 5; cond {}|};
  [%expect {|
    : Incorrect statement |}]
;;

let%expect_test "stmt if with else that is a block" =
  pp print_stmt parse_stmt {|if cond {} else {}|};
  [%expect {|
    if cond {} else {} |}]
;;

let%expect_test "stmt if with else that is another if" =
  pp print_stmt parse_stmt {|if cond {} else if cond2 {}|};
  [%expect {|
    if cond {} else if cond2 {} |}]
;;

let%expect_test "stmt if with wrong else" =
  pp print_stmt parse_stmt {|if cond {} else do_smth()|};
  [%expect {|
    : Incorrect statement |}]
;;

(********** for **********)

let%expect_test "stmt empty for" =
  pp print_stmt parse_stmt {|for {}|};
  [%expect {|
    for {} |}]
;;

let%expect_test "stmt for with only conition" =
  pp print_stmt parse_stmt {|for a > 0 {}|};
  [%expect {|
    for a > 0 {} |}]
;;

let%expect_test "stmt empty for with semicolons" =
  pp print_stmt parse_stmt {|for ;; {}|};
  [%expect {|
    for {} |}]
;;

let%expect_test "stmt simple for" =
  pp print_stmt parse_stmt {|for i := 0; i < 10; i++ {}|};
  [%expect {|
    for i := 0; i < 10; i++ {} |}]
;;

let%expect_test "stmt for with range and number" =
  pp print_stmt parse_stmt {|for range 10 {}|};
  [%expect {|
    for i := 0; i < 10; i++ {} |}]
;;

(********** range **********)

let%expect_test "stmt range with decl only index" =
  pp print_stmt parse_stmt {|for i := range array {}|};
  [%expect {|
    for i := range array {} |}]
;;

let%expect_test "stmt range with decl index and elem" =
  pp print_stmt parse_stmt {|for i, elem := range array {}|};
  [%expect {|
    for i, elem := range array {} |}]
;;

let%expect_test "stmt range with decl blank index and elem" =
  pp print_stmt parse_stmt {|for _, _ := range array {}|};
  [%expect {|
    for _, _ := range array {} |}]
;;

let%expect_test "stmt range with assign only index" =
  pp print_stmt parse_stmt {|for i = range array {}|};
  [%expect {|
    for i = range array {} |}]
;;

let%expect_test "stmt range with assign index and elem" =
  pp print_stmt parse_stmt {|for i, elem = range array {}|};
  [%expect {|
    for i, elem = range array {} |}]
;;

let%expect_test "stmt range with assign blank index and elem" =
  pp print_stmt parse_stmt {|for _, _ = range array {}|};
  [%expect {|
    for _, _ = range array {} |}]
;;

let%expect_test "stmt range with more than two idents" =
  pp print_stmt parse_stmt {|for a, b, c, d = range array {}|};
  [%expect {| : Incorrect statement |}]
;;

let%expect_test "stmt range without idents" =
  pp print_stmt parse_stmt {|for := range array {}|};
  [%expect {| : Incorrect statement |}]
;;

let%expect_test "stmt range with const array" =
  pp print_stmt parse_stmt {|for i, elem := range [3]int{1, 2, 3} {}|};
  [%expect {|
    for i, elem := range [3]int{1, 2, 3} {} |}]
;;
