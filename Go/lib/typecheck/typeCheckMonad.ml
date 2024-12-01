(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast
open TypeCheckErrors

type error = TypeCheckError of type_check_error [@@deriving show { with_path = false }]

module Ident = struct
  type t = ident

  let compare = compare
end

module MapIdent = Map.Make (Ident)

module BaseMonad = struct
  type ('st, 'a) t = 'st -> 'st * ('a, error) Result.t

  let return : 'a -> ('st, 'a) t = fun x st -> st, Result.Ok x
  let fail : 'a -> ('st, 'b) t = fun e st -> st, Result.Error e

  let ( >>= ) : ('st, 'a) t -> ('a -> ('st, 'b) t) -> ('st, 'b) t =
    fun x f st ->
    let st1, x1 = x st in
    match x1 with
    | Result.Ok x -> f x st1
    | Result.Error x -> fail x st1
  ;;

  let ( *> ) : ('st, 'a) t -> ('st, 'b) t -> ('st, 'b) t = fun x1 x2 -> x1 >>= fun _ -> x2

  let ( >>| ) : ('st, 'a) t -> ('a -> 'b) -> ('st, 'b) t =
    fun x f st ->
    let st, x = x st in
    match x with
    | Result.Ok x -> return (f x) st
    | Result.Error er -> fail er st
  ;;

  let iter : ('a -> ('st, unit) t) -> 'a list -> ('st, unit) t =
    fun f list ->
    let f acc el = acc *> f el *> return () in
    List.fold_left f (return ()) list
  ;;

  let map : ('a -> ('st, 'b) t) -> 'a list -> ('st, 'b list) t =
    fun f list ->
    let f acc el = acc >>= fun acc -> f el >>= fun el -> return (el :: acc) in
    List.fold_left f (return []) list >>| List.rev
  ;;

  let read : ('st, 'st) t = fun st -> return st st
  let write : 'st -> ('st, _) t = fun st_new _ -> st_new, Result.Ok ()
  let run : ('st, 'a) t -> 'st -> 'st * ('a, error) Result.t = fun f st -> f st
end

type global_env = ident MapIdent.t
type local_env = ident MapIdent.t
type type_check = global_env * local_env
