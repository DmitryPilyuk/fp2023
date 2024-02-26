(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

let%expect_test _ =
parse_with_print {| let f x = match x with 
  | 0 -> "zero"
  | _ -> "not zero"
  |};
  [%expect
    {|
    [(EDeclaration ("f",
        (EFun ((PVal "x"),
           (EMatchWith ((EIdentifier "x"),
              [((PConst (Int 0)), (EConst (String "zero")));
                (PAny, (EConst (String "not zero")))]
              ))
           )),
        None))
      ] |}]
;;