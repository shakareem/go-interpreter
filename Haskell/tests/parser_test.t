Copyright 2024, Kostya Oreshin and Nikita Shchutskii
SPDX-License-Identifier: MIT

  $ ../bin/REPL.exe -dparsetree < fac.txt
  (FunBind (((Ident "fac"), None), ([], (PIdentificator (Ident "n")), None),
     [],
     (OrdBody
        ((IfThenEsle (
            ((Binop (((Identificator (Ident "n")), None), Less,
                ((Const (Int 0)), None))),
             None),
            ((OptionBld Nothing), None),
            ((OptionBld
                (Just
                   ((FunctionApply (((Identificator (Ident "save_fac")), None),
                       ((Identificator (Ident "n")), None), [])),
                    None))),
             None)
            )),
         None)),
     [(FunBind (((Ident "save_fac"), None),
         ([], (PIdentificator (Ident "y")), None), [],
         (Guards (
            (((Binop (((Identificator (Ident "y")), None), Equality,
                 ((Const (Int 0)), None))),
              None),
             ((Const (Int 1)), None)),
            [(((Const (Bool true)), None),
              ((Binop (((Identificator (Ident "y")), None), Multiply,
                  ((FunctionApply (((Identificator (Ident "save_fac")), None),
                      ((Binop (((Identificator (Ident "y")), None), Minus,
                          ((Const (Int 1)), None))),
                       None),
                      [])),
                   None)
                  )),
               None))
              ]
            )),
         []))
       ]
     ))