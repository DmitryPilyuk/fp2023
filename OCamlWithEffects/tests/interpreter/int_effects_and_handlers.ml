(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

(* Factorial. *)

let%expect_test _ =
interpret
    {| 
      effect E : char -> int effect ;;
      effect G : int -> int effect ;;

      let f = 
        try perform E 'a' with
        | (G x) k -> continue k (x / 5)

    |};
  [%expect {|
    Error: no suitable handler was found for effect 'E'. |}]
;;

let%expect_test _ =
interpret
    {| 
      effect E : char -> int effect ;;
      effect G : int -> int effect ;;

      let f x = 
        try perform x with
        | (E x) k -> match x with | '0' -> continue k 0 | '1' -> continue k 1 | _ -> continue k 2
        | (G x) k -> continue k (x / 5)
      ;;
      
      let res1 = f (E '0')
      let res2 = f (E '1')
      let res3 = f (E '9')
      let res4 = f (G 26)

    |};
  [%expect {|
    val E : char -> int effect = <effect>
    val G : int -> int effect = <effect>
    val f : 'b effect -> 'b = <fun>
    val res1 : int = 0
    val res2 : int = 1
    val res3 : int = 2
    val res4 : int = 5 |}]
;;

(* ---------------- *)