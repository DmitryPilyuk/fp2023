(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Typedtree

type error =
  [ `Occurs_check (* Trying to unify two types when one of the types contains the other type as a subtype or variable. *)
  | `Unification_failed of typ * typ (* Castable types are not compatible. *)
  | `Unbound_variable of id (* An undeclared variable is used. *)
  | `Unbound_effect of id (* An undeclared effect is used. *)
  | `Not_reachable (* Sections of code that will never be reached. *)
  | `Several_bounds of id (* A type variable is assigned more than one constraint. *)
  | `Not_effect_in_handler
  | `Wrong_effect_typ of id * typ
  | `Not_effect_with_args of id
  | `Perform_with_no_effect
  ]

(* Constructors for types *)

let occurs_check = `Occurs_check
let unification_failed ty1 ty2 = `Unification_failed (ty1, ty2)
let unbound_variable name = `Unbound_variable name
let unbound_effect name = `Unbound_effect name
let not_reachable = `Not_reachable
let several_bounds name = `Several_bounds name
let not_effect_in_handler = `Not_effect_in_handler
let wrong_effect_type name typ = `Wrong_effect_typ (name, typ)
let not_effect_with_args name = `Not_effect_with_args name
let perform_with_no_effect = `Perform_with_no_effect
(* ---------------- *)