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

let%expect_test _ =
  interpret
    {| let rec fib n = if n > 1 then fib (n - 1) + fib (n - 2) else 1;;
let res = fib 5;;|};
  [%expect {|
    val fib : int -> int = <fun>
    val res : int = 8 |}]
;;

let%expect_test _ =
  interpret
    {| let list_map f =
      let rec helper l = match l with
      | [] -> []
      | h :: tl -> (f h) :: helper tl
    in helper 
    ;;

    let increment x = x + 1;;

    let test_list = [2; 4; 5; 3; 6; 3;];;

    let res = list_map increment test_list
    |};
  [%expect
    {|
    val list_map : ('e -> 'f) -> 'm list -> 'n list = <fun>
    val increment : int -> int = <fun>
    val test_list : int list = [2; 4; 5; 3; 6; 3]
    val res : 'n list = [3; 5; 6; 4; 7; 4] |}]
;;

let%expect_test _ =
  interpret
    {| effect NotDigit: char -> int effect

    let int_of_char c = match c with
    | '0' -> 0
    | '1' -> 1 
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | c -> perform (NotDigit c)
    

let rec sum_up li = match li with
| [] -> 0
| h :: tl -> int_of_char h + sum_up tl


let test_l = ['1'; 'a'; '0'; '1'; '5'; '7'; 'v'; '2'; '9']

let res = try sum_up test_l with
| (NotDigit x) k -> continue k 0
|};
  [%expect
    {|
    val NotDigit : char -> int effect = <effect>
    val int_of_char : char -> int = <fun>
    val sum_up : char list -> int = <fun>
    val test_l : char list = ['1'; 'a'; '0'; '1'; '5'; '7'; 'v'; '2'; '9']
    val res : int = 25 |}]
;;
