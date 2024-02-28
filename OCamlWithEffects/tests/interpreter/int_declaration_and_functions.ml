(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

let%expect_test _ =
  interpret
    {| fun x -> x + 1 |};
  [%expect {| - : int -> int = <fun> |}]
;;

let%expect_test _ =
  interpret
    {| ((fun x -> x + 1) 1) >= 2 |};
  [%expect {| - : bool = true |}]
;;

let%expect_test _ =
  interpret
    {| (fun x -> x) (fun x -> x + 1) 1 |};
  [%expect {| - : int = 2 |}]
;;