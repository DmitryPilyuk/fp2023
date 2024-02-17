(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Values
open Interpreter
open Typedtree
open Pprint

let pp_value ppf typ =
  let rec helper ppf = function
  | VInt i -> Format.fprintf ppf "%d" i
  | VBool b -> Format.fprintf ppf "%B" b
  | VChar c -> Format.fprintf ppf "'%c'" c
  | VString s -> Format.fprintf ppf "%S" s
  | VUnit -> Format.fprintf ppf "()"
  | VTuple l ->
      Format.fprintf
        ppf
        "@[<hov>(%s)@]"
        (String.concat ", " (List.map (fun v -> Format.asprintf "%a" helper v) l))
  | VList l ->
      Format.fprintf
        ppf
        "@[<hov>[%s]@]"
        (String.concat "; " (List.map (fun v -> Format.asprintf "%a" helper v) l))
  | VFun _ -> Format.fprintf ppf "<fun>"
  | VRecFun _ -> Format.fprintf ppf "<fun>"
in 
helper ppf typ
;;

let pp_error ppf error =
  match error with
  | `Devision_by_zero -> Format.fprintf ppf "Division by zero."
  | `Unbound_variable id -> Format.fprintf ppf "Unbound variable '%s'." id
  | `Non_existen_operation -> Format.fprintf ppf "Non-existent operation."
  | `Non_existen_type -> Format.fprintf ppf "Non-existent type."
  | `Type_error -> Format.fprintf ppf "Type error."
;;

let print_interpreter_error e =
  let error_str = Format.asprintf "%a" pp_error e in
  Format.printf "%s\n" error_str
;;

let value_to_string value = Format.asprintf "%a" pp_value value

let print_expr_value value typ = Format.printf "%s = %s \n" (expr_without_name typ) (value_to_string value)

let print_program_value val_env typ_env names_list =
  Base.List.iter names_list ~f:(fun name ->
    let typ = Base.Map.find typ_env name in
    let value = Base.Map.find val_env name in
    match (typ, value) with
    | Some (Scheme (_, typ)), Some value -> Format.printf "%s = %s\n" (expr_with_name name typ) (value_to_string value)
    | _, _ -> Printf.printf "") (* Unreacheable *)
;;
