(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving show { with_path = false }]

type const =
  | Char of char
  | String of string
  | Int of int
  | Bool of bool
  | Unit
[@@deriving show { with_path = false }]

type bin_op =
  | Plus
  | Sub
  | Mul
  | Div
  | Eq
  | NEq
  | Gt
  | Gte
  | Lt
  | Lte
  | And
  | Or
[@@deriving show { with_path = false }]

type un_op =
  | Not
  | Minus
[@@deriving show { with_path = false }]

type expr =
  | EConst of const
  | EBinaryOperation of bin_op * expr * expr
  | EUnaryOperation of un_op * expr
  | EIdentifier of id
  | EApplication of expr * expr
  | EFun of id list * expr
  | EDeclaration of id * id list * expr
  | ERecDeclaration of id * id list * expr
  | EIfThenElse of expr * expr * expr
  | EList of expr list
  | ETuple of expr list
  | EMatchWith of expr * (expr * expr) list
[@@deriving show { with_path = false }]

type program = expr list [@@deriving show { with_path = false }]
