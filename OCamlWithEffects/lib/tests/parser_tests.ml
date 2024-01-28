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
    [(ERecDeclaration ("fact", ["n"],
        (EIfThenElse (
           (EBinaryOperation (Eq, (EIdentifier "n"), (EConst (Int 1)))),
           (EConst (Int 1)),
           (EBinaryOperation (Mul, (EIdentifier "n"),
              (EApplication ((EIdentifier "fact"),
                 (EBinaryOperation (Sub, (EIdentifier "n"), (EConst (Int 1))))))
              ))
           ))
        ))
      ] |}]
;;
