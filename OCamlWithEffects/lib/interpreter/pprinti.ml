(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Values
open Interpreter

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

let print_expr_value expr =
  match InterpreterR.int_expr expr with
  | Ok (env, value) ->
    let typ_str = Format.asprintf "%a" pp_value value in
    Format.printf "%s\n" typ_str
  | Error x ->
    let error_str = Format.asprintf "%a" pp_error x in
    Format.printf "%s\n" error_str
;;