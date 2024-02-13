(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree
open Errors
open Inferencer

let pp_const_type ppf typ =
  match typ with
  | Int -> Format.fprintf ppf "int"
  | Bool -> Format.fprintf ppf "bool"
  | Unit -> Format.fprintf ppf "unit"
  | Char -> Format.fprintf ppf "char"
  | String -> Format.fprintf ppf "string"
;;

let pp_type ppf typ =
  let rec helper ppf = function
    | TPrim t -> pp_const_type ppf t
    | TVar v -> 
      let var_name = Char.chr (Char.code 'a' + v) in
      Format.fprintf ppf "'%c" var_name
    | TArr (l, r) ->
      let pp = 
        match l with
        | TArr (_, _) -> Format.fprintf ppf "(%a) -> %a" (helper) l (helper) r
        | _ -> Format.fprintf ppf "%a -> %a" (helper) l (helper) r
      in pp
    | TList t ->
      let pp = 
        match t with
        | TArr (_, _) -> Format.fprintf ppf "(%a) list" helper t
        | _ -> Format.fprintf ppf "%a list" helper t
      in pp
    | TTuple tl ->
      Format.fprintf
        ppf
        "%a"
        (Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.fprintf ppf " * ")
          (fun ppf ty -> helper ppf ty))
        tl
    | TEffect t -> Format.fprintf ppf "%a effect" helper t
  in 
  helper ppf typ
;;

let pp_error ppf error =
  match error with
  | `Occurs_check -> Format.fprintf ppf "Occurs check failed."
  | `Not_reachable -> Format.fprintf ppf "Not reachable."
  | `Unification_failed (l, r) -> 
      Format.fprintf
        ppf
        "Unification failed: type %a does not match expected type %a"
        pp_type l
        pp_type r
  | `Unbound_variable name ->
      Format.fprintf ppf "Unbound variable '%s'" name
  | `Several_bounds name ->
      Format.fprintf ppf "Variable '%s' is bound several times" name
;;

let print_expr_type expr =
  match run_expr_inferencer expr with
  | Ok typ ->
    let typ_str = Format.asprintf "%a" pp_type typ in
    Format.printf "%s\n" typ_str
  | Error x ->
    let error_str = Format.asprintf "%a" pp_error x in
    Format.printf "%s\n" error_str
;;

let print_program_type program =
  match run_program_inferencer program with
  | Ok typ_list ->
    List.iter (fun (name, typ) -> 
      let typ_str = Format.asprintf "%a" pp_type typ in
      match name with
      | "" -> Format.printf "- : %s\n" typ_str
      | _ -> Format.printf "val %s : %s\n" name typ_str
    ) typ_list
  | Error e ->
    let error_str = Format.asprintf "%a" pp_error e in
    Format.printf "%s\n" error_str
;;
