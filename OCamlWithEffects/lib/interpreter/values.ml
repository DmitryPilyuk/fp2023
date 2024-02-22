(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

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
  | VRecFun of id * value
  | VEffectDeclaration of id
  | VEffectWithArguments of id * value
  | VEffectWithoutArguments of id
  | VEffectContinue of continue_val
  | VThrowingValue of value
  | VHandlerWithoutContinue of pattern (* ??? *)

and enviroment = (string, value, Base.String.comparator_witness) Base.Map.t
and handlers = (string, (pattern * expr * continue_val), Base.String.comparator_witness) Base.Map.t

(* Constructors for values *)
let vint c = VInt c
let vbool c = VBool c
let vchar c = VChar c
let vstring c = VString c
let vunit = VUnit
let vtuple l = VTuple l
let vlist l = VList l
let vfun pat exp env = VFun (pat, exp, env)
let vrecfun ident v = VRecFun (ident, v)
let veffect_declaration n = VEffectDeclaration n
let veffect_with_arguments n a = VEffectWithArguments (n, a)
let veffect_without_arguments n = VEffectWithoutArguments n
let veffect_continue k = VEffectContinue k
let vhandler_without_continue p = VHandlerWithoutContinue p
let vthrowing_value v = VThrowingValue v
(* ---------------- *)
