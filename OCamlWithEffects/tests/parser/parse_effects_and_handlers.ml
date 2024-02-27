(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

let%expect_test _ =
parse_with_print
    {|
    effect E1 : int -> int effect
  |};
  [%expect
    {|
    [(EEffectDeclaration ("E1", (AArrow (AInt, (AEffect AInt)))))] |}]
;;

let%expect_test _ =
parse_with_print
    {|
    try perform E1 x with
    | E (a :: b) k -> continue k 0
    | E (a :: b :: c) k -> continue k 1
    | E x -> continue k 2
  |};
  [%expect
    {|
    Syntax error. |}]
;;

let%expect_test _ =
parse_with_print
    {|
    let f1 = perform E
    let f2 = perform E x
  |};
  [%expect
    {|
    [(EDeclaration ("f1", (EEffectPerform (EEffectWithoutArguments "E")), None));
      (EDeclaration ("f2",
         (EEffectPerform (EEffectWithArguments ("E", (EIdentifier "x")))), None))
      ] |}]
;;

let%expect_test _ =
parse_with_print
    {|
    effect E1 : int -> int effect
    effect E2 : int -> int effect

    let f x = 
      try x with
      | E1 k -> continue k 0
      | E2 k -> continue k 5
    ;;

    let res = f (perform (E1)) in res
  |};
  [%expect
    {|
    [(EEffectDeclaration ("E1", (AArrow (AInt, (AEffect AInt)))));
      (EEffectDeclaration ("E2", (AArrow (AInt, (AEffect AInt)))));
      (EDeclaration ("f",
         (EFun ((PVal "x"),
            (ETryWith ((EIdentifier "x"),
               [(EffectHandler ((PEffectWithoutArguments "E1"),
                   (EEffectContinue ((Continue "k"), (EConst (Int 0)))),
                   (Continue "k")));
                 (EffectHandler ((PEffectWithoutArguments "E2"),
                    (EEffectContinue ((Continue "k"), (EConst (Int 5)))),
                    (Continue "k")))
                 ]
               ))
            )),
         None));
      (EDeclaration ("res",
         (EApplication ((EIdentifier "f"),
            (EEffectPerform (EEffectWithoutArguments "E1")))),
         (Some (EIdentifier "res"))))
      ] |}]
;;
