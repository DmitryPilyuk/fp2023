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

and enviroment = (string, value, Base.String.comparator_witness) Base.Map.t

(* Constructors for values *)
let vint c = VInt c
let vbool c = VBool c
let vchar c = VChar c
let vstring c = VString c
let vunit = VUnit
let vtuple l = VTuple l
let vlist l = VList l
let vfun pat exp env = VFun (pat, exp, env)
(* ---------------- *)