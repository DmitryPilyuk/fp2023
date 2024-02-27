(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

(* List construction pattern *)

let%expect_test _ =
  interpret {|
    let f x = 
      (match x with
      | [] -> 0
      | hd :: tl -> hd)
    ;;

    let res1 = f []
    let res2 = f [1 ; 2 ; 3]
    let res3 = f (1 :: 2 :: 3 :: [])
  |};
  [%expect {|
    val f : int list -> int = <fun>
    val res1 : int = 0
    val res2 : int = 1
    val res3 : int = 1 |}]
;;

let%expect_test _ =
  interpret {|
    let f x = 
      (match x with
      | [] -> 0
      | hd :: snd :: tl -> hd)
    in
    f [1]
  |};
  [%expect {|
    Pattern matching failure: the value does not match any pattern. |}]
;;

(* ---------------- *)

(* List construction pattern *)

let%expect_test _ =
  interpret {|
    let f x = 
      (match x with
      | (x, 0) -> x
      | (x, 1) -> x + 1
      | _ -> 0)
    ;;

    let res1 = f (1, 2)
    let res2 = f (1, 0)
    let res3 = f (1, 1)
  |};
  [%expect {|
    val f : int * int -> int = <fun>
    val res1 : int = 0
    val res2 : int = 1
    val res3 : int = 2 |}]
;;

let%expect_test _ =
  interpret {|
    let f x = 
      (match x with
      | (x, 0) -> x
      | (x, 1) -> x + 1)
    in
    f (1, 2)
  |};
  [%expect {|
    Pattern matching failure: the value does not match any pattern. |}]
;;

(* ---------------- *)