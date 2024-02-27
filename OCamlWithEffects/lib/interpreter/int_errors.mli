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
val division_by_zero : [> `Division_by_zero ]
val pattern_matching_failure : [> `Pattern_matching_failure ]
val unbound_variable : 'a -> [> `Unbound_variable of 'a ]
val unbound_effect : 'a -> [> `Unbound_effect of 'a ]
val non_existen_operation : [> `Non_existen_operation ]
val non_existen_type : [> `Non_existen_type ]
val type_error : [> `Type_error ]
val unbound_handler : 'a -> [> `Unbound_handler of 'a ]
val handler_without_continue : 'a -> [> `Handler_without_continue of 'a ]
val not_continue_var : 'a -> [> `Not_continue_var of 'a ]
