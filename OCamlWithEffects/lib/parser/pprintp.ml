(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Errorsp

let pp_error ppf error =
  match error with
  | Syntax_error -> Format.fprintf ppf "Syntax error."
;;

let print_parser_error e =
  let error_str = Format.asprintf "%a" pp_error e in
  Format.printf "%s\n" error_str