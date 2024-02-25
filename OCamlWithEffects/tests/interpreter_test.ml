(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

(* Simple expressions tests*)

let%expect_test _ =
  interpret {| 1 + (-2) * 5 - 10 / 2 |};
  [%expect {| - : int = -14 |}]
;;

let%expect_test _ =
  interpret {| not (5 = 2) && 6 > 1 || 5 <= 1 |};
  [%expect {| - : bool = true |}]
;;

(* ---------------- *)

let%expect_test _ =
  interpret
    {| let rec fact n = if n = 1 then 1 else n * fact (n - 1);;
let res = fact 5;;|};
  [%expect {|
    val fact : int -> int = <fun>
    val res : int = 120 |}]
;;
