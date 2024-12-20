(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Typecheck
open Parse

let pp str =
  match parse parse_file str with
  | Ok res -> TypeChecker.pp res
  | Error _ -> print_endline ": syntax error"
;;

(********** main func **********)

let%expect_test "ok: single main" =
  pp {|
    func main() {}
    |};
  [%expect {|
    CORRECT |}]
;;

let%expect_test "err: multiple main" =
  pp {|
    func main() {}
    func main() {}
    |};
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Multiple declaration error: main is redeclared in func() |}]
;;

let%expect_test "err: main with returns" =
  pp {|
  func main() bool {}
  |};
  [%expect
    {| ERROR WHILE TYPECHECK WITH Incorrect main error: func main must have no arguments and no return values |}]
;;

let%expect_test "err: main with args" =
  pp {|
  func main(a int) {}
  |};
  [%expect
    {| ERROR WHILE TYPECHECK WITH Incorrect main error: func main must have no arguments and no return values |}]
;;

let%expect_test "err: no main" =
  pp {|
  var a int
  func foo(a, a, b int) {}
  |};
  [%expect {| ERROR WHILE TYPECHECK WITH Incorrect main error: main func not found |}]
;;

let%expect_test "ok: main call" =
  pp {|
    func foo() {
        main()
    }

    func main() {}
    |};
  [%expect {|
    CORRECT |}]
;;

(********** top var decl **********)

let%expect_test "ok: single var decl no type with simple init " =
  pp {|
  var a = 5

  func main() {}
  |};
  [%expect {| CORRECT |}]
;;

let%expect_test "ok: single var decl with type and right init " =
  pp {|
  var a int = 5

  func main() {}
  |};
  [%expect {| CORRECT |}]
;;

let%expect_test "ok: single var decl with type and wrong init " =
  pp {|
  var a int = ""

  func main() {}
  |};
  [%expect {| ERROR WHILE TYPECHECK WITH Mismatched types: Types mismatched in binoper |}]
;;

let%expect_test "ok: func call init with right number of elements" =
  pp
    {|
    var a, b, c  = get3()

    func get3() (int, int, int) {
        return 1, 2, 3
    }
    
    func main() {}
    |};
  [%expect {| CORRECT |}]
;;

let%expect_test "err: func call one init with mismatched number of elements" =
  pp {|
    var a, b, c  = get0()

    func get0() {}
    
    func main() {}
    |};
  [%expect
    {| ERROR WHILE TYPECHECK WITH Mismatched types: Function without returns in expression |}]
;;

let%expect_test "err: func call one init with mismathced types" =
  pp
    {|
    var a, b, c bool = get3()

    func get3() (int, int, int) {
        return 1, 2, 3
    }
    
    func main() {}
    |};
  [%expect {| ERROR WHILE TYPECHECK WITH Mismatched types: multiple return types mismatched |}]
;;

let%expect_test "err: var redeclaration" =
  pp {|
    var a = 0

    var a = ""
    s
    func main() {}
    |};
  [%expect {| : syntax error |}]
;;

(********** top func decl **********)

let%expect_test "ok: simple func" =
  pp {|
    func foo() {}

    func main() {}
    |};
  [%expect {|
    CORRECT |}]
;;

let%expect_test "ok: id func " =
  pp {|
    func id(a int) int {
        return a
    }

    func main() {}
    |};
  [%expect {|
    CORRECT |}]
;;

let%expect_test "err: repeated idents in args" =
  pp {|
    func foo(a, a int) {}

    func main() {}
    |};
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Multiple declaration error: a is redeclared in int |}]
;;

let%expect_test "err: func redeclaration" =
  pp
    {|
    func foo(a int) {}

    func foo() int {
        return 5
    }

    func main() {}
    |};
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Multiple declaration error: foo is redeclared in func() int |}]
;;

let%expect_test "err: func arg redeclaration" =
  pp {|
    func foo(a int) {
        var a int
    }

    func main() {}
    |};
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Multiple declaration error: a is redeclared in int |}]
;;

let%expect_test "err: var and func with the same name" =
  pp {|
    var foo int

    func foo() {}

    func main() {}
    |};
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Multiple declaration error: foo is redeclared in int |}]
;;

let%expect_test "ok: correct declarations #1" =
  pp
    {|
    func main() {}

    func foo(a int, b int, c int) bool {}

    func foo1(a int, b int, c int) bool {}   |};
  [%expect {| CORRECT |}]
;;

let%expect_test "ok: factorial func" =
  pp
    {|
    func main() {
        fac(6)
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

(********** stmt **********)

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

let%expect_test "arg not declared" =
  pp {|
    func main() {
        println(a)
    }

    func println(a int) {}
  |};
  [%expect {| ERROR WHILE TYPECHECK WITH Undefined ident error: a is not defined |}]
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
