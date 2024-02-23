(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type error =
  [ `Devision_by_zero
  | `Unbound_variable of id
  (* | `Unbound_effect of id *)
  | `Unbound_handler of id
  | `Non_existen_operation
  | `Non_existen_type
  (* | `Not_exhaustive_match *)
  | `Type_error
  | `Handler_without_continue of v
  ]

(* Constructors for types *)
let devision_by_zero = `Devision_by_zero
let unbound_variable name = `Unbound_variable name
let non_existen_operation = `Non_existen_operation
let non_existen_type = `Non_existen_type
let type_error = `Type_error
let unbound_handler name = `Unbound_handler name
let handler_without_continue v = Handler_without_continue v
(* ---------------- *)