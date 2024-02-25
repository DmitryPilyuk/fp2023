(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

(* Const tests *)
let%expect_test _ =
  inference {| let x = 1 |};
  [%expect {|
    val x : int |}]
;;

let%expect_test _ =
  inference {| let x = 'a' |};
  [%expect {|
    val x : char |}]
;;

let%expect_test _ =
  inference {| let x = "string" |};
  [%expect {|
    val x : string |}]
;;

let%expect_test _ =
  inference {| let x = true |};
  [%expect {|
    val x : bool |}]
;;

let%expect_test _ =
  inference {| let x = ()|};
  [%expect {|
    val x : unit |}]
;;

(* ---------------- *)

(* List and tuple tests *)

let%expect_test _ =
  inference {| let x = ["Hello"; "World"] |};
  [%expect {| val x : string list |}]
;;

let%expect_test _ =
  inference {| let x = [[('a', 1); ('b', 5)]; [('c', 23); ('d', 11)]] |};
  [%expect {| val x : char * int list list |}]
;;

let%expect_test _ =
  inference {| let x = (true, "Yes", 25 * 3) |};
  [%expect {| val x : bool * string * int |}]
;;

(* ---------------- *)

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

(* Polymorphysm tests *)

let%expect_test _ =
  inference {| let id x = x |};
  [%expect {| val id : 'a -> 'a |}]
;;

let%expect_test _ =
  inference {| let f g x y = g x y |};
  [%expect {| val f : ('b -> 'c -> 'e) -> 'b -> 'c -> 'e |}]
;;

let%expect_test _ =
  inference {| let f x y z= let id x = x in [id x; id y; id z] |};
  [%expect {| val f : 'e -> 'e -> 'e -> 'e list |}]
;;

let%expect_test _ =
  inference {| let f x = let helper a b = (a, b) in helper x |};
  [%expect {| val f : 'a -> 'e -> 'a * 'e |}]
;;

(* ---------------- *)
