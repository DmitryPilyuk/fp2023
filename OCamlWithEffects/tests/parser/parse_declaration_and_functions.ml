(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

(* Let *)

let%expect_test _ =
   parse_with_print {| let x = (1) |};
  [%expect {|
    [(EDeclaration ("x", (EConst (Int 1)), None))] |}]
;;

let%expect_test _ =
parse_with_print {| let f x y = x + y 
let main = f 4 6|};
  [%expect
    {|
  [(EDeclaration ("f",
      (EFun ((PVal "x"),
         (EFun ((PVal "y"),
            (EBinaryOperation (Add, (EIdentifier "x"), (EIdentifier "y")))))
         )),
      None));
    (EDeclaration ("main",
       (EApplication ((EApplication ((EIdentifier "f"), (EConst (Int 4)))),
          (EConst (Int 6)))),
       None))
    ] |}]
;;

(* ---------------- *)

(* Let Rec *)

let%expect_test _ =
parse_with_print {| let rec f x = f (x - 1)|};
  [%expect
    {|
    [(ERecDeclaration ("f",
        (EFun ((PVal "x"),
           (EApplication ((EIdentifier "f"),
              (EBinaryOperation (Sub, (EIdentifier "x"), (EConst (Int 1))))))
           )),
        None))
      ] |}]
;;

(* ---------------- *)

(* Let/Let Rec with IN *)

let%expect_test _ =
parse_with_print{| let f = 
    let x = 5 in
    1 + x * 3
  |};
  [%expect
    {|
    [(EDeclaration ("f",
        (EDeclaration ("x", (EConst (Int 5)),
           (Some (EBinaryOperation (Add, (EConst (Int 1)),
                    (EBinaryOperation (Mul, (EIdentifier "x"), (EConst (Int 3))))
                    )))
           )),
        None))
      ] |}]
;;

(* ---------------- *)

(* Functions *)

let%expect_test _ =
parse_with_print{|
   fun x ->
      (match x with
      | [] -> 0
      | hd :: tl -> 1)
  |};
  [%expect
    {|
    [(EFun ((PVal "x"),
        (EMatchWith ((EIdentifier "x"),
           [(PNill, (EConst (Int 0)));
             ((PListCons ((PVal "hd"), (PVal "tl"))), (EConst (Int 1)))]
           ))
        ))
      ] |}]
;;

(* ---------------- *)