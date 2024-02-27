(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

(* List contructions *)

(* ---------------- *)

(* List *)

(* ---------------- *)

(* Tuple *)

let%expect_test _ =
  interpret {| 
    let f = (1, 2 , 3)
  |};
  [%expect {|
    val f : int * int * int = (1, 2, 3) |}]
;;

let%expect_test _ =
  interpret {| 
    let f = (1, (1, 2 :: 3 :: 4 :: 5 :: []) , "some string")
  |};
  [%expect {|
    val f : int * int * int list * string = (1, (1, [2; 3; 4; 5]), "some string") |}]
;;

(* ---------------- *)
