module Export where

    import Ast as A
    import Text.Regex

    printAst :: A.Prog -> String
    printAst a = case a of
        (A.Prog []) -> ""
        (A.Prog (x:xs)) -> printDec x ++ printAst (A.Prog xs)

    printDec :: A.Dec -> String
    printDec d = (
        case d of
          A.FuncDec {name=name', params=p, rtype=rt, body=b} ->
                let result = case rt of
                               Just n -> n
                               Nothing -> "void"
                in (result ++ " " ++ name' ++
                    "(" ++ printParams p ++ ") \n{\n  " ++
                     addIndents (printExp b) ++ "\n}")
          A.Typedef {type1=typ1, type2=typ2}
                -> "typedef " ++ typ1 ++ " " ++ typ2 ++ ";"
          A.Ifdef {mode=str} -> "#ifndef " ++ str
          A.Define {defname=n, value=val} -> "#define " ++ n ++ " " ++ val
          A.Include {file=s} -> "#include " ++ s
        ) ++ "\n\n"

    printParams :: [A.Param] -> String
    printParams p = case p of
        [] -> ""
        [x] -> printParam x
        (x:xs) -> printParam x ++ ", " ++ printParams xs

    printParam :: A.Param -> String
    printParam (A.Param {pvar=v, ptyp=t}) = t ++ " " ++ v

    printExp :: A.Exp -> String
    printExp e = case e of
        A.Seq s -> case s of
            [] -> ""
            [s] -> printExp s
            (x:xs) -> printExp x ++ "\n" ++ printExp (A.Seq xs)
        A.VarExp x -> printVar x
        A.VarDec x-> printVar x ++ ";"
        A.IntExp x -> show x
        A.FuncApply {func=f, args=a} -> f ++ "(" ++ printArgs a ++ ")"
        A.OpExp {left=l, oper=o, right=r}
            -> printExp l ++ " " ++ printOp o ++ " " ++ printExp r
        A.Negate v -> "-" ++ printVar v
        A.Assign {var=v, val=exp, op=o} -> printVar v ++ case o of
                Just x -> " " ++ printOp x ++ "= "
                Nothing -> " = "
            ++ printExp exp ++ ";"
        A.Typecast {tvar=v, newtyp=t}
            -> "(" ++ t ++ ") " ++ printExp v
        A.Parens e -> "(" ++ printExp e ++ ")"
        A.Return s -> "return " ++ printName s ++ ";"
        A.Newline -> ""

    printName :: A.Exp -> String
    printName s = case s of
        A.VarDec x -> v x
        _ -> printExp s

    printVar :: A.Var -> String
    printVar var = case typ var of
                        Just n -> n ++ " "
                        Nothing -> ""
                   ++ v var ++
                   case idx var of
                        Just x -> "[" ++ x ++ "]"
                        Nothing -> ""

    printOp :: A.Op -> String
    printOp op = case op of
        Plus ->  "+"
        Minus -> "-"
        Times -> "*"
        And ->  "&"
        Or -> "|"
        ExOr -> "^"
        LShift -> "<<"
        RShift -> ">>"

    printArgs :: [A.Exp] -> String
    printArgs [] = ""
    printArgs [x] = printExp x
    printArgs (x:xs) = printExp x ++ " ," ++ printArgs xs

    addIndents :: String -> String
    addIndents s = subRegex (mkRegex "\n") s "\n  "

