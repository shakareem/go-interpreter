(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Typecheck
open Parse

let pp str =
  match parse parse_file str with
  | Ok res -> TypeChecker.pp res
  | Error _ -> print_endline ": syntax error"
;;

let%expect_test "multiple func declaration" =
  pp {|
    func main() {}
    func main() {}
    |};
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Multiple declaration error: main is redeclared in func() |}]
;;

let%expect_test "multiple declaration in args" =
  pp {|
    func main() {}
    func foo(a, a, b int) {}
    |};
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Multiple declaration error: a is redeclared in int |}]
;;

let%expect_test "incorrect long_var_decl with different lengths" =
  pp
    {|
    var a, foo, c  = get()

    var x int

    func main() {}

    func get() (int, int, int) {} |};
  [%expect {| CORRECT |}]
;;

let%expect_test "multiple declaration in global space" =
  pp
    {|
    var a, foo, c  = foo()

    var x int

    func main() {}

    func foo() (int, int, int) {} |};
  [%expect
    {| ERROR WHILE TYPECHECK WITH Multiple declaration error: foo is redeclared in int |}]
;;

let%expect_test "correct declarations #1" =
  pp
    {|
    func main() {}

    func foo(a int, b int, c int) bool {}

    func foo1(a int, b int, c int) bool {}   |};
  [%expect {| CORRECT |}]
;;

let%expect_test "incorrect declaration type" =
  pp
    {|
    var a, b, c <-chan [5]int = get()

    var x int

    func main() {}

    func foo1(a1 int, c int, b int) bool {}

    func foo2(a1 int, c int, b int) bool {}  |};
  [%expect {| ERROR WHILE TYPECHECK WITH Mismatched types: <-chan [5]int |}]
;;

let%expect_test "undefined var inc" =
  pp
    {|
    var x int

    func main() {}

    func foo(a1 int, c int, b int) bool {
        a2++
    }  
    |};
  [%expect {| ERROR WHILE TYPECHECK WITH Undefined ident error: a2 is not defined |}]
;;

let%expect_test "undefined func call" =
  pp
    {|
    var x int

    func main() {
        foo2()
    }

    func foo(a1 int, c int, b int) bool {}  |};
  [%expect {| ERROR WHILE TYPECHECK WITH Undefined ident error: foo2 is not defined |}]
;;

let%expect_test "multiple declarations in func body with args" =
  pp
    {|
    var x int

    func foo(a2 int) bool {
        var a2 int
    }

    func main() {}

    func get() int {}
    |};
  [%expect
    {| ERROR WHILE TYPECHECK WITH Multiple declaration error: a2 is redeclared in int |}]
;;

let%expect_test "main with returns and args" =
  pp
    {|
    var a, b, c <-chan [5]int = get()

    var x int

    func main(a int) bool {}

    func main1(a1 int, c int, b int) bool {}
  |};
  [%expect
    {| ERROR WHILE TYPECHECK WITH Incorrect main error: func main must have no arguments and no return values |}]
;;

let%expect_test "arg not declared" =
  pp
    {|
    func main() {
        {
            println(a)
        }
    }

    func println() {}
  |};
  [%expect {| ERROR WHILE TYPECHECK WITH Undefined ident error: a is not defined |}]
;;

let%expect_test "no main func" =
  pp
    {|
    var main, b, c <-chan [5]int = get()

    var x int

    func main4(a int) bool {}

    func main1(a1 int, c int, b int) bool {}
  |};
  [%expect {| ERROR WHILE TYPECHECK WITH Incorrect main error: main func not found |}]
;;

let%expect_test "correct fac" =
  pp
    {|
    func main() {
        {
            fac(6)
        }
    }

    func fac(n int) int {
        if n == 1 {
            return 1
        } else {
            return n * fac(n - 1)
        }
    }
  |};
  [%expect {| CORRECT |}]
;;

let%expect_test "unknown var in if cond" =
  pp
    {|
    func main() {
        {
            fac(6)
        }
    }

    func fac(n int) int {
        if a == 1 {
            return 1
        } else {
            return n * fac(n - 1)
        }
    }
  |};
  [%expect {| ERROR WHILE TYPECHECK WITH Undefined ident error: a is not defined |}]
;;

let%expect_test "mismatched types in binop" =
  pp
    {|
    var a = 5

    var b = "st"

    func test() {
        return
    }

    func pritln(a int) int {
        return a
    }

    func id(a int) int {
        return a
    }

    var f int

    func main() {
        defer test()
        var c = a + b
        go println(id(10))
    }
|};
  [%expect {| ERROR WHILE TYPECHECK WITH Mismatched types: Types mismatched in binoper |}]
;;

let%expect_test "mismatched type in decl # 1" =
  pp
    {|
    var a = 5

    var b = "st"

    func test() {
        return
    }

    func pritln(a int) int {
        return a
    }

    func id(a int) int {
        return a
    }

    var f int

    func main() {
        defer test()
        var c = a + b
        go println(id(10))
    }
  |};
  [%expect {| ERROR WHILE TYPECHECK WITH Mismatched types: Types mismatched in binoper |}]
;;

let%expect_test "mismatched type in decl # 2" =
  pp
    {|
    var a = "s"

    var b = 5

    func test() {
        return
    }

    func println(a int) int {
        return a
    }

    func id(a int) int {
        return a
    }

    var f int

    func main() {
        defer test()
        var c = a + b
        go println(id(10))
    }
|};
  [%expect {| ERROR WHILE TYPECHECK WITH Mismatched types: Types mismatched in binoper |}]
;;

let%expect_test "mismatched type in func_call" =
  pp
    {|
    var a = 5

    var b = 5

    func test() {
        return
    }

    func println(a int) int {
        return a
    }

    func id(a string) string {
        return a
    }

    var f int

    func main() {
        defer test()
        var c = a + id("st")
        go println(id(10))
    }
|};
  [%expect {| ERROR WHILE TYPECHECK WITH Mismatched types: Types mismatched in binoper |}]
;;

let%expect_test "correct #3" =
  pp
    {|
    var a = 5

    var b int = 5

    func test() {
        return
    }

    func println(a int) int {
        return a
    }

    func id(a int) int {
        return a
    }

    var f int

    func main() {
        defer test()
        var c = a + b
        go println(id(10))
    }
|};
  [%expect {| CORRECT |}]
;;

let%expect_test "return type of func mismatch" =
  pp
    {|
    var a = 5

    var b int = 5

    func test() {
        return
    }

    func println(a string) int {
        return a
    }

    func id(a int) int {
        return a
    }

    var f int

    func main() {
        defer test()
        var c = a + b
        go println(id(10))
    }
|};
  [%expect {| ERROR WHILE TYPECHECK WITH Mismatched types: Types mismatched in binoper |}]
;;

let%expect_test "return with empty func returns" =
  pp {|
    func main() {}

    func foo(a int, b int) {
        return 5
    }
|};
  [%expect {| ERROR WHILE TYPECHECK WITH Mismatched types: func return types mismatch |}]
;;

let%expect_test "correct anon_func" =
  pp
    {|
    func s(a string) {}

    func main() {
        value := func(a string) {
            g := func(a string) {
                s("Test")
            }
            s("Test")
            g("2")
        }
        value("4")
    }
|};
  [%expect {| CORRECT |}]
;;
