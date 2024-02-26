(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

let%expect_test _ =
inference
  {|
    effect E : int effect
  |};
  [%expect {| val E : int effect |}]
;;

let%expect_test _ =
inference
  {|
    effect E : int
  |};
  [%expect {| Effect E is of type int, but type int effect was expected. |}]
;;

let%expect_test _ =
  inference
  {|
    effect E : (int -> bool) -> (char -> string) -> (int->bool) effect
  |};
  [%expect {| val E : (int -> bool -> char -> string) -> (int -> bool) effect |}]
;;

let%expect_test _ =
  inference
  {|
    effect E : (int -> bool) -> (char -> string) -> (int->bool)
  |};
  [%expect {| Effect E is of type (int -> bool -> char -> string) -> int -> bool, but type (int -> bool -> char -> string) -> (int -> bool) effect was expected. |}]
;;

let%expect_test _ =
  inference
    {|
    let f x = 
      match x with
      | E -> 0
      | _ -> 0
    ;;
    |};
  [%expect {| Unbound effect 'E' |}]
;;