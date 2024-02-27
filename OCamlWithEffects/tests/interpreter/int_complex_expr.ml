(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

(* Factorial. *)

let%expect_test _ =
  interpret
    {| let rec fact n = if n = 1 then 1 else n * fact (n - 1);;
let res = fact 5;;|};
  [%expect {|
    val fact : int -> int = <fun>
    val res : int = 120 |}]
;;

(* ---------------- *)

(* Fibonacci. *)

let%expect_test _ =
  interpret
    {| let rec fib n = if n > 1 then fib (n - 1) + fib (n - 2) else 1;;
let res = fib 5;;|};
  [%expect {|
    val fib : int -> int = <fun>
    val res : int = 8 |}]
;;

(* ---------------- *)