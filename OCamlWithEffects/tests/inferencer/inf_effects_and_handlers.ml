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
    effect E : int * int -> int effect
  |};
  [%expect {| val E : int * int -> int effect |}]
;;

let%expect_test _ =
  inference
  {|
    effect E : int * int -> int
  |};
  [%expect {| Effect E is of type int * int -> int, but type int * int -> int effect was expected. |}]
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

let%expect_test _ =
  inference {| 
    effect E : int effect ;;

    let f = 
      try perform E with
      | E k -> continue k 1
  |};
  [%expect {|
    val E : int effect
    val f : int |}]
;;

let%expect_test _ =
  inference {| 
    let f x = 
      try x with
      | 1 k -> continue k 0
    ;;
  |};
  [%expect {|
    Left side of effect handler can only contain an effect with continue point. |}]
;;

let%expect_test _ =
  inference {| 
    effect E : int effect ;;

    let f = 
      try perform E with
      | (E x) k -> continue k 1
  |};
  [%expect {|
    Effect 'E' cannot take arguments - it is an effect without arguments. |}]
;;

let%expect_test _ =
  inference {| 
    effect E : int -> int effect ;;

    let f = 
      try perform (E 0) with
      | E k -> continue k 1
  |};
  [%expect {|
    Effect 'E' is an effect that takes an argument, but it's presented without an argument in the handler. |}]
;;

let%expect_test _ =
  inference {| 
    effect E : int -> int effect ;;

    let f = 
      try perform (E 0) with
      | (E x) k -> 0
  |};
  [%expect {|
    The effect handler does not contain a continuation. |}]
;;

let%expect_test _ =
  inference {| 
    effect E : int -> int effect ;;

    let l = 0 ;; 

    let f = 
      try perform (E 0) with
      | (E x) k -> continue l 1
  |};
  [%expect {|
    Variable 'l' is not continue variable. |}]
;;

let%expect_test _ =
  inference {| 
    effect E : int -> int effect ;;

    let f = 
      try perform (E 0) with
      | (E x) k -> continue l 1
  |};
  [%expect {|
    Unbound variable 'l' |}]
;;

let%expect_test _ =
  inference {| 
    effect E : int effect ;;

    let f = 0;;

    let res = 
      try perform f with
      | E k -> continue k 0
  |};
  [%expect {|
    The type of the argument passed to perform must be the effect typ. |}]
;;