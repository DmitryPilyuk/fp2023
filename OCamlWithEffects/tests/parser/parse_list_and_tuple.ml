(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

(* List *)

let%expect_test _ =
parse_with_print {| let x = [20; 24; 5] |};
  [%expect
    {|
    [(EDeclaration ("x",
        (EList [(EConst (Int 20)); (EConst (Int 24)); (EConst (Int 5))]), None))
      ] |}]
;;

(* ---------------- *)

(* List construction *)

let%expect_test _ =
parse_with_print {| let x = 1 :: 2 :: [] |};
  [%expect
    {|
    [(EDeclaration ("x",
        (EListCons ((EConst (Int 1)), (EListCons ((EConst (Int 2)), (EList [])))
           )),
        None))
      ] |}]
;;

(* ---------------- *)

(* Tuple *)

let%expect_test _ =
parse_with_print {| let x = (1, 2, 3, 4) |};
  [%expect
    {|
    [(EDeclaration ("x",
        (ETuple
           [(EConst (Int 1)); (EConst (Int 2)); (EConst (Int 3));
             (EConst (Int 4))]),
        None))
      ] |}]
;;

(* let%expect_test _ =
parse_with_print {| let x = 1,2 |};
  [%expect
    {|
    [(EDeclaration ("x", (ETuple [(EConst (Int 1)); (EConst (Int 2))]), None))] |}]
;; *)

(* ---------------- *)