(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type value =
  | VInt of int
  | VBool of bool
  | VChar of char
  | VString of string
  | VUnit
  | VTuple of value list
  | VList of value list
  | VFun of pattern * expr * enviroment
  | VRecFun of string * value
  | VEffectDeclaration of string
  | VEffectWithArguments of string * value
  | VEffectWithoutArguments of string
  | VEffectContinue of continue_val
  | VThrowingValue of value

and enviroment = (string, value, Base.String.comparator_witness) Base.Map.t

and handlers =
  (string, pattern * expr * continue_val, Base.String.comparator_witness) Base.Map.t

val vint : int -> value
val vbool : bool -> value
val vchar : char -> value
val vstring : string -> value
val vunit : value
val vtuple : value list -> value
val vlist : value list -> value
val vfun : pattern -> expr -> enviroment -> value
val vrecfun : string -> value -> value
val veffect_declaration : string -> value
val veffect_with_arguments : string -> value -> value
val veffect_without_arguments : string -> value
val veffect_continue : continue_val -> value
val vthrowing_value : value -> value
