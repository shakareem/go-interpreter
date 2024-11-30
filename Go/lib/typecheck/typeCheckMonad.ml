(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast

module BaseMonad = struct
  type ('st, 'err, 'a) t = 'st -> 'st * ('err, 'a) Result.t

  let return : 'a -> ('st, 'err, 'a) t = fun x st -> st, Result.ok x

  let ( >>= ) : ('st, 'err, 'a) t -> ('a -> ('st, 'err, 'b) t) -> ('st, 'err, 'b) t =
    fun x f st ->
    let st1, x1 = x st in
    match x1 with
    | Result.Ok x -> f x st1
    | Result.Error x -> st1, Result.error x
  ;;

  let error : 'err -> ('st, 'err, 'a) t = fun err st -> st, Result.error err
end

module TypeCheckerMonad = struct
  include BaseMonad
end
