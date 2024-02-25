(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parser
open Pprint
open Pprinti
open Pprintp
open Inferencer
open Interpreter
open Errorsp
open Auxiliary

let inference_program ast =
  let typ = 
    (match run_program_inferencer ast with
    | Ok (env, names_list) -> print_program_type env names_list
    | Error e -> print_inferencer_error e)
  in typ
;;

let inference_expr ast =
  let typ =
      (match run_expr_inferencer ast with
      | Ok typ -> print_expr_type typ
      | Error e -> print_inferencer_error e)
    in typ
;;

let inference program =
  let ast = parse program in
  let res =
    match ast with
    | Ok ast -> 
      (match determine_ast_type ast with
      | FreeExpression -> 
        (match ast with
        | [x] -> inference_expr x
        | _ -> print_parser_error (syntax_error)) (* Unreachable *)
      | DeclarationList -> inference_program ast
      | MixedList -> print_parser_error (syntax_error))
    | Error _ -> print_parser_error (syntax_error)
  in res
;;

let interpret_program ast =
  let res = 
    (match run_program_inferencer ast with
    | Ok (typ_env, names_list) -> 
      (match run_program_interpreter ast with
      | Ok val_env -> print_program_value val_env typ_env names_list
      | Error e -> print_interpreter_error e)
    | Error e -> print_inferencer_error e)
  in res
;;

let interpret_expr ast =
  let res = 
    (match run_expr_inferencer ast with
    | Ok typ -> 
      (match run_expr_interpreter ast with
      | Ok value -> print_expr_value value typ
      | Error e -> print_interpreter_error e)
    | Error e -> print_inferencer_error e)
  in res
;;

let interpret program =
  let ast = parse program in
  let res =
    match ast with
    | Ok ast -> 
      (match determine_ast_type ast with
      | FreeExpression -> 
        (match ast with
        | [x] -> interpret_expr x
        | _ -> print_parser_error (syntax_error)) (* Unreachable *)
      | DeclarationList -> interpret_program ast
      | MixedList -> print_parser_error (syntax_error))
    | Error _ -> print_parser_error (syntax_error)
  in res
;;

let interpret_program1 ast =
  let res = 
      (match run_program_interpreter ast with
      | Ok val_env -> print_program_value1 ["f"; "res" ; "g" ; "z" ; "E"] val_env 
      | Error e -> print_interpreter_error e)
  in res
;;

let interpret_expr1 ast =
  let res = 
      (match run_expr_interpreter ast with
      | Ok value -> print_expr_value1 "a" value
      | Error e -> print_interpreter_error e)
  in res
;;

let interpret1 program =
  let ast = parse program in
  let res =
    match ast with
    | Ok ast -> 
      (match determine_ast_type ast with
      | FreeExpression -> 
        (match ast with
        | [x] -> interpret_expr1 x
        | _ -> print_parser_error (syntax_error)) (* Unreachable *)
      | DeclarationList -> interpret_program1 ast
      | MixedList -> print_parser_error (syntax_error))
    | Error _ -> print_parser_error (syntax_error)
  in res
;;