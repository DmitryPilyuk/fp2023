(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib

let print_parse_result s =
  match Parser.parse s with
  | Result.Ok ast -> Format.printf "%a\n" Ast.pp_program ast
  | Error _ -> Format.printf "Error"
;;

let%expect_test _ =
  print_parse_result {| let rec fact n = if n = 1 then 1 else n * fact (n - 1) |};
  [%expect
    {|
    [(ERecDeclaration ("fact",
        (EFun ((PVal "n"),
           (EIfThenElse (
              (EBinaryOperation (Eq, (EIdentifier "n"), (EConst (Int 1)))),
              (EConst (Int 1)),
              (EBinaryOperation (Mul, (EIdentifier "n"),
                 (EApplication ((EIdentifier "fact"),
                    (EBinaryOperation (Sub, (EIdentifier "n"), (EConst (Int 1))))
                    ))
                 ))
              ))
           )),
        None))
      ] |}]
;;

let%expect_test _ =
  print_parse_result {| let x = [20; 24; 5] |};
  [%expect
    {|
    [(EDeclaration ("x",
        (EList [(EConst (Int 20)); (EConst (Int 24)); (EConst (Int 5))]), None))
      ] |}]
;;

let%expect_test _ =
  print_parse_result {| let f x y = x + y 
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

let%expect_test _ =
  print_parse_result {| let x = (1, 2, 3, 4) |};
  [%expect
    {|
    [(EDeclaration ("x",
        (ETuple
           [(EConst (Int 1)); (EConst (Int 2)); (EConst (Int 3));
             (EConst (Int 4))]),
        None))
      ] |}]
;;

let%expect_test _ =
  print_parse_result {| let x = (1) |};
  [%expect {|
    [(EDeclaration ("x", (EConst (Int 1)), None))] |}]
;;

let%expect_test _ =
  print_parse_result {| let f x = match x with 
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

let%expect_test _ =
  print_parse_result {| let f = 
    let x = 5 in
    1 + x * 3
  |};
  [%expect {|
    [(EDeclaration ("f",
        (EDeclaration ("x", (EConst (Int 5)),
           (Some (EBinaryOperation (Add, (EConst (Int 1)),
                    (EBinaryOperation (Mul, (EIdentifier "x"), (EConst (Int 3))))
                    )))
           )),
        None))
      ] |}]
;;

let%expect_test _ =
  print_parse_result {| let x = (1,2) |};
  [%expect
    {|
    [(EDeclaration ("x", (ETuple [(EConst (Int 1)); (EConst (Int 2))]), None))] |}]
;;
