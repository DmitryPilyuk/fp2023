(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Values

module type MONAD_ERROR = sig
  type('a, 'e) t

  val return : 'a -> ('a, 'e) t
  val fail : 'e -> ('a, 'e) t
  val (>>=) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val (let*) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t (* возможно вынести в модуль Syntax *)
end
