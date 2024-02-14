(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parser
open Pprint
open Pprinti
open Typedtree
open Inferencer
open Interpreter

(* let run_parser program = parse program

let ast program = 
  match run_parser program with
  | Ok ast -> ast
  | Error e -> Format.printf "%s\n" e
;;

let run_inferencer ast =
  match ast with
  | hd :: tl -> run_program_inferencer ast
  | expr -> [ run_expr_inferencer expr ]
;;

let run program =
  let ast = ast program in
  let inferencer_result = run_inferencer ast in
  print_program_type inferencer_result
;; *)

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

let run_interpret expr =
  let ast = parse expr in
  let typ =
    match ast with
    | Ok ast -> print_expr_value (List.hd ast)
    | Error e -> Format.printf "%s\n" e
  in typ
;;

let run_interpret2 expr =
  let ast = parse expr in
  let typ =
    match ast with
    | Ok ast -> run_program_print ast
    (* | Error e -> Format.printf "%s\n" e *)
  in typ
;;