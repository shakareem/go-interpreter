(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

module Ident : sig
  type t = Ast.ident

  val compare : 'a -> 'a -> int
end

module MapIdent : sig
  type key = Ident.t
  type 'a t = 'a Stdlib__Map.Make(Ident).t

  val empty : 'a t
end

type ctype =
  | Ctype of Ast.type'
  | Ctuple of Ast.type' list (** Used to check multiple returns of a function *)

val equal_ctype : ctype -> ctype -> Ppx_deriving_runtime.bool

(** MapIdent is used to map ident and it's type in global space *)
type global_env = ctype MapIdent.t

(** list of MapIdent is used to map ident and it's type in local space.
    Add MapIdent if you enter in if/for body or func literal and then delete it after checking block of statements
    If we didn't find ident in Map, we will seek it in next 'till we find it or not find it even in global space
    and fail with undefined ident error *)
type local_env = ctype MapIdent.t list

(** List of ctype that stores function return types, used to check returns in nested functions *)
type current_funcs = ctype list

(** Current typechecker state *)
type type_check = global_env * local_env * current_funcs

module CheckMonad : sig
  type 'a t = (type_check, 'a) BaseMonad.t

  val return : 'a -> 'a t
  val fail : Errors.error -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( *> ) : 'a t -> 'b t -> 'b t
  val iter : ('a -> unit t) -> 'a list -> unit t
  val iter2 : ('a -> 'b -> unit t) -> 'a list -> 'b list -> unit t
  val map : ('a -> 'b t) -> 'a list -> 'b list t
  val run : 'a t -> type_check -> type_check * ('a, Errors.error) Result.t

  (** Looking for definition of ident in all lists of local_env, return firs one*)
  val seek_local_definition_ident : MapIdent.key -> ctype option t

  (** Add returning match while entering new func space*)
  val write_func : ctype -> unit t

  (** Remove returning match while entering new func space*)
  val delete_func : unit t

  (** Read type by ident in global space*)
  val read_global_ident : MapIdent.key -> ctype option t

  (** Save ident-type in local space*)
  val save_local_ident : MapIdent.key -> ctype -> unit t

  (** Save ident-type in global space*)
  val save_global_ident : MapIdent.key -> ctype -> unit t

  (** Retrun ident in local and global space or fail*)
  val retrieve_ident : MapIdent.key -> ctype t

  (** Used to match return types in current function*)
  val get_func_return_type : ctype t

  (** Add new Map to local_env while entering a new block/anon_func/if body/for body*)
  val write_env : unit t

  (** Remove Map from local_env while leaving block/anon_func/if body/for body*)
  val delete_env : unit t

  (** Print ctype to string*)
  val print_type : ctype -> string
end
