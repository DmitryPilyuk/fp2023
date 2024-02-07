  $ dune exec demoFact
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
    ]
