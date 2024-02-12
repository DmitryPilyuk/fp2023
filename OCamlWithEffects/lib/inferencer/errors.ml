(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Typedtree

type error =
  [ `Occurs_check
  | `Unification_failed of typ * typ
  | `Unbound_variable of id
  | `Not_reachable
  | `Several_bounds of id
  ]

(* Constructors for types *)
let occurs_check = `Occurs_check
let unification_failed ty1 ty2 = `Unification_failed (ty1, ty2)
let unbound_variable name = `Unbound_variable name
let not_reachable = `Not_reachable
let several_bounds name = `Several_bounds name
(* ---------------- *)