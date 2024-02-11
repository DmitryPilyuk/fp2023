(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parser
open Pprint
open Typedtree
open Inferencer

let run program =
  let ast = parse program in
  let typ = 
    match ast with
    | Ok ast -> print_program_type ast
    | Error e -> Format.printf "%s\n" e
  in typ
;;

let run_expr expr =
  let ast = parse expr in
  let typ =
    match ast with
    | Ok ast -> print_expr_type (List.hd ast)
    | Error e -> Format.printf "%s\n" e
  in typ
;;
