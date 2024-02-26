(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

(* Function inference tests *)

let%expect_test _ =
  inference {| let f x = x + 1|};
  [%expect {| val f : int -> int |}]
;;

let%expect_test _ =
  inference {| let f x = x :: [2]|};
  [%expect {| val f : int -> int list |}]
;;

let%expect_test _ =
  inference {| let rec fact n = if n = 1 then 1 else n * fact (n - 1) |};
  [%expect {| val fact : int -> int |}]
;;

let%expect_test _ =
  inference {| let f g x y = if g x then x * y else y |};
  [%expect {| val f : (int -> bool) -> int -> int -> int |}]
;;

(* ---------------- *)