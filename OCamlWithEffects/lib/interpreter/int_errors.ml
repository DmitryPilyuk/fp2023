(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type error =
  [ `Devision_by_zero
  | `Pattern_matching_failure
  | `Unbound_variable of id
  | `Unbound_effect of id
  | `Unbound_handler of id
  | `Non_existen_operation
  | `Non_existen_type
  | `Type_error
  | `Handler_without_continue of id
  | `Not_continue_var of id 
  ]

(* Constructors for types *)
let division_by_zero = `Division_by_zero
let pattern_matching_failure = `Pattern_matching_failure
let unbound_variable name = `Unbound_variable name
let unbound_effect name = `Unbound_effect name
let non_existen_operation = `Non_existen_operation
let non_existen_type = `Non_existen_type
let type_error = `Type_error
let unbound_handler name = `Unbound_handler name
let handler_without_continue name = `Handler_without_continue name
let not_continue_var name = `Not_continue_var name
(* ---------------- *)
