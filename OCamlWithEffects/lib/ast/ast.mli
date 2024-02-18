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
  | Add
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
  | Plus
[@@deriving show { with_path = false }]

type pattern =
  | PAny
  | PNill
  | PConst of const
  | PVal of id
  | PTuple of pattern list
  | PListCons of pattern * pattern
  | EffectPattern of effect_pattern
[@@deriving show { with_path = false }]

and effect_pattern =
  | PEffectWithArguments of id * pattern
  | PEffectWithoutArguments of id
  | PEffectHolder of pattern * id
[@@deriving show { with_path = false }]

type effect_types_annotation =
  | AInt
  | ABool
  | AChar
  | AString
  | AUnit
  | AArrow of effect_types_annotation * effect_types_annotation
  | ATuple of effect_types_annotation list
  | AList of effect_types_annotation
  | AEffect of effect_types_annotation
[@@deriving show { with_path = false }]

type expr =
  | EConst of const
  | EBinaryOperation of bin_op * expr * expr
  | EUnaryOperation of un_op * expr
  | EIdentifier of id
  | EApplication of expr * expr
  | EFun of pattern * expr
  | EDeclaration of id * expr * expr option
  | ERecDeclaration of id * expr * expr option
  | EEffectDelaration of id * effect_types_annotation
  | EIfThenElse of expr * expr * expr
  | EList of expr list
  | EListCons of expr * expr
  | ETuple of expr list
  | EMatchWith of expr * (pattern * expr) list
  | EEffectWithArguments of id * expr
  | EEffectWithoutArguments of id
  | EEffectPerform of expr
  | EEffectContinue of id * expr
[@@deriving show { with_path = false }]

type program = expr list [@@deriving show { with_path = false }]
