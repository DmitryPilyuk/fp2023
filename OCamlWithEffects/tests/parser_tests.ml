(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib

let print_parse_result s =
  match Parser.parse s with
  | Result.Ok ast -> Format.printf "%a\n" Ast.pp_program ast
  | Error _ -> Format.printf "Syntax error."
;;

(* Declaration parser tests *)

let%expect_test _ =
  print_parse_result {| let rec f x = f (x - 1)|};
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

let%expect_test _ =
  print_parse_result {| let f = 
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

(* ---------------- *)

(* Function and application parser tests *)

(* ---------------- *)

(* List and tuple parser tests *)

let%expect_test _ =
  print_parse_result {| let x = [20; 24; 5] |};
  [%expect
    {|
    [(EDeclaration ("x",
        (EList [(EConst (Int 20)); (EConst (Int 24)); (EConst (Int 5))]), None))
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
  print_parse_result {| let x = (1,2) |};
  [%expect
    {|
    [(EDeclaration ("x", (ETuple [(EConst (Int 1)); (EConst (Int 2))]), None))] |}]
;;

(* ---------------- *)

(* Pattern parser tests *)

(* ---------------- *)

(* Effect annotation parser tests *)

(* ---------------- *)

(* MatchWith parser tests *)

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

(* ---------------- *)

(* TryWith and Effects parser tests *)

let%expect_test _ =
  print_parse_result
    {|
      effect E : char -> int effect

      let helper x =
         match x with
         | 'a' -> perform E 'a'
         | _ -> 0
      ;;

      let f = try helper 'b' with
      | E k -> continue k 3
      | E x k -> continue k 2
   |};
  [%expect {| Syntax error. |}]
;;

let%expect_test _ =
  print_parse_result
    {|
   effect DevisionByZero : int effect

   let helper x y = 
      match y with
      | 0 -> perform DevisionByZero
      | other -> x / other
   ;;

   let div x y =
      try helper x y with
      | DevisionByZero k -> continue k 0
   ;;
  |};
  [%expect
    {|
    [(EEffectDeclaration ("DevisionByZero", (AEffect AInt)));
      (EDeclaration ("helper",
         (EFun ((PVal "x"),
            (EFun ((PVal "y"),
               (EMatchWith ((EIdentifier "y"),
                  [((PConst (Int 0)),
                    (EEffectPerform (EEffectWithoutArguments "DevisionByZero")));
                    ((PVal "other"),
                     (EBinaryOperation (Div, (EIdentifier "x"),
                        (EIdentifier "other"))))
                    ]
                  ))
               ))
            )),
         None));
      (EDeclaration ("div",
         (EFun ((PVal "x"),
            (EFun ((PVal "y"),
               (ETryWith (
                  (EApplication (
                     (EApplication ((EIdentifier "helper"), (EIdentifier "x"))),
                     (EIdentifier "y"))),
                  [(EffectHandler ((PEffectWithoutArguments "DevisionByZero"),
                      (EEffectContinue ((Continue "k"), (EConst (Int 0)))),
                      (Continue "k")))
                    ]
                  ))
               ))
            )),
         None))
      ] |}]
;;

let%expect_test _ =
  print_parse_result
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

let%expect_test _ =
  print_parse_result
    {|
    let f x = 
      try x with
      | E k -> 5
      | x k -> continue k 0
    |};
  [%expect
    {|
    [(EDeclaration ("f",
        (EFun ((PVal "x"),
           (ETryWith ((EIdentifier "x"),
              [(EffectHandler ((PEffectWithoutArguments "E"), (EConst (Int 5)),
                  (Continue "k")));
                (EffectHandler ((PVal "x"),
                   (EEffectContinue ((Continue "k"), (EConst (Int 0)))),
                   (Continue "k")))
                ]
              ))
           )),
        None))
      ] |}]
;;

let%expect_test _ =
  print_parse_result
    {| effect E: string -> int effect;;

let binary_int_of_str n = match n with
| "0" -> 0
| "1" -> 1 
| s -> perform (E s);;

let rec sum_up li = match li with
| [] -> 0
| s :: ss -> binary_int_of_str s + sum_up ss;;

let test_l = ["1"; "a"; "0"; "1"];;
let res = try sum_up test_l with
| E k -> continue k 0 in
res;;|};
  [%expect
    {|
    [(EEffectDeclaration ("E", (AArrow (AString, (AEffect AInt)))));
      (EDeclaration ("binary_int_of_str",
         (EFun ((PVal "n"),
            (EMatchWith ((EIdentifier "n"),
               [((PConst (String "0")), (EConst (Int 0)));
                 ((PConst (String "1")), (EConst (Int 1)));
                 ((PVal "s"),
                  (EEffectPerform (EEffectWithArguments ("E", (EIdentifier "s")))))
                 ]
               ))
            )),
         None));
      (ERecDeclaration ("sum_up",
         (EFun ((PVal "li"),
            (EMatchWith ((EIdentifier "li"),
               [(PNill, (EConst (Int 0)));
                 ((PListCons ((PVal "s"), (PVal "ss"))),
                  (EBinaryOperation (Add,
                     (EApplication ((EIdentifier "binary_int_of_str"),
                        (EIdentifier "s"))),
                     (EApplication ((EIdentifier "sum_up"), (EIdentifier "ss")))
                     )))
                 ]
               ))
            )),
         None));
      (EDeclaration ("test_l",
         (EList
            [(EConst (String "1")); (EConst (String "a")); (EConst (String "0"));
              (EConst (String "1"))]),
         None));
      (EDeclaration ("res",
         (ETryWith (
            (EApplication ((EIdentifier "sum_up"), (EIdentifier "test_l"))),
            [(EffectHandler ((PEffectWithoutArguments "E"),
                (EEffectContinue ((Continue "k"), (EConst (Int 0)))),
                (Continue "k")))
              ]
            )),
         (Some (EIdentifier "res"))))
      ] |}]
;;

(* ---------------- *)

(* Binary, unary operation, ifthenelse and other parser tests *)

let%expect_test _ =
  print_parse_result {| let x = (1) |};
  [%expect {|
    [(EDeclaration ("x", (EConst (Int 1)), None))] |}]
;;

let%expect_test _ =
  print_parse_result {| 1 + 5 * 3 |};
  [%expect
    {|
    [(EBinaryOperation (Add, (EConst (Int 1)),
        (EBinaryOperation (Mul, (EConst (Int 5)), (EConst (Int 3))))))
      ] |}]
;;

let%expect_test _ =
  print_parse_result {| 1 * (+5) / (-3) |};
  [%expect
    {|
    [(EBinaryOperation (Div,
        (EBinaryOperation (Mul, (EConst (Int 1)),
           (EUnaryOperation (Plus, (EConst (Int 5)))))),
        (EUnaryOperation (Minus, (EConst (Int 3))))))
      ] |}]
;;

let%expect_test _ =
  print_parse_result {| if false || true && not false then "Yes" else "No" |};
  [%expect
    {|
    [(EIfThenElse (
        (EBinaryOperation (Or, (EConst (Bool false)),
           (EBinaryOperation (And, (EConst (Bool true)),
              (EUnaryOperation (Not, (EConst (Bool false))))))
           )),
        (EConst (String "Yes")), (EConst (String "No"))))
      ] |}]
;;

(* ---------------- *)

(* General tests *)

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

(* ---------------- *)
