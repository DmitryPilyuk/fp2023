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
  [%expect {|
    [(ERecDeclaration ("f",
        (EFun ((PVal "x"),
           (EApplication ((EIdentifier "f"),
              (EBinaryOperation (Sub, (EIdentifier "x"), (EConst (Int 1))))))
           )),
        None))
      ] |}]
  
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

(* let%expect_test _ =
  print_parse_result {|
  effect EmptyListException : int
  ;;
  let list_hd x = 
    match x with
    | [] -> perform EmptyListException
    | hd :: _ -> hd
  ;;
  let empty = []
  ;;
  let non_empty = [1; 2; 3]
  ;;
  let safe_list_hd l = try list_hd l with
   | EmptyListException k -> (0, false)
  ;;
  let empty_hd = safe_list_hd empty
  ;;
  let non_empty_hd = safe_list_hd non_empty
  ;;
  |};
  [%expect {|
    [(EEffectDeclaration ("EmptyListException", AInt));
      (EDeclaration ("list_hd",
         (EFun ((PVal "x"),
            (EMatchWith ((EIdentifier "x"),
               [(PNill,
                 (EEffectPerform (EEffectWithoutArguments "EmptyListException")));
                 ((PListCons ((PVal "hd"), PAny)), (EIdentifier "hd"))]
               ))
            )),
         None));
      (EDeclaration ("empty", (EList []), None));
      (EDeclaration ("non_empty",
         (EList [(EConst (Int 1)); (EConst (Int 2)); (EConst (Int 3))]), None));
      (EDeclaration ("safe_list_hd",
         (EFun ((PVal "l"),
            (ETryWith (
               (EApplication ((EIdentifier "list_hd"), (EIdentifier "l"))),
               [(EffectHandler ((PEffectWithoutArguments "EmptyListException"),
                   (ETuple [(EConst (Int 0)); (EConst (Bool false))]),
                   (Continue "k")))
                 ]
               ))
            )),
         None));
      (EDeclaration ("empty_hd",
         (EApplication ((EIdentifier "safe_list_hd"), (EIdentifier "empty"))),
         None));
      (EDeclaration ("non_empty_hd",
         (EApplication ((EIdentifier "safe_list_hd"), (EIdentifier "non_empty"))),
         None))
      ] |}]
;; *)

let%expect_test _ =
  print_parse_result {|
    effect E1 : int -> int effect
    effect E2 : int -> int effect

    let f x = 
      try x with
      | E1 k -> continue k 0
      | E2 k -> continue k 5
    ;;

    let res = f (perform (E1)) in res
  |};
  [%expect {|
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
  print_parse_result {|
    let f x = 
      try x with
      | E k -> 5
      | x k -> continue k 0
    |};
  [%expect {|
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
(* ---------------- *)

(* Binary, unary operation, ifthenelse and other parser tests *)

let%expect_test _ =
  print_parse_result {| let x = (1) |};
  [%expect {|
    [(EDeclaration ("x", (EConst (Int 1)), None))] |}]
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
