module Export where
    import Ast as A
    import Text.RegEx

    printAst :: A.Prog -> String
    printAst a = case a of
        (A.Prog []) -> ""
        (A.Prog (x:xs)) = printDec x ++ printAst (A.Prog xs)

    printDec :: A.Dec -> String
    printDec d = (case d of
        A.Func {name=n, params=p, rtype=rt, body=b} ->
            let r = case rt of
                Just n -> n + " "
                Nothing -> "void "
            in (rt ++ n ++ "(" ++ printParams p ++ ") {\n" ++
                addIndents (printExp b) ++ "\n}")
        A.Typedef -> "typedef" ++ type1 d ++ " " ++ type2 d ++ ";"
        A.Define -> "#define " ++ printVar (name d) ++ " " ++ printExp (val d)
        A.Include -> "#include \"" ++ file d  ++ "\""
    ) ++ "\n\n"

    printParams :: [A.Param] -> String
    printParams p = case p of
        (A.Param []) -> ""
        (A.Param [x]) -> printParam x
        (A.Param (x:xs)) -> printParam x ++ ", " ++ printParams xs

    printParam :: A.Param -> String
    printParam {pvar=v, ptype=t} = t ++ " " ++ v

    printExp :: A.Exp -> String
    printExp e = case e of
        A.Seq s -> case s of
            [] -> ""
            [s] -> printExp s ++ ";"
            (x:xs) -> printExp x ++ printExp (A.Seq xs)
        A.Var x -> printVar e
        A.VarDec {vars=vs, typ=t} -> t ++ " " ++ printArgs vs
        A.IntExp x -> Show x
        A.FuncApply {func=f, args=a} -> f ++ "(" ++ printArgs a ++ ")"
        A.OpExp {left=l, op=o, right=r}
            -> printExp l ++ " " ++ printOp o ++ " " ++ printExp r
        A.Negate v = "-" ++ printVar v
        A.Assign {var=v, val=exp, op=o} -> printVar v ++ case o of
                Just x -> printOp x + "="
                Nothing -> "="
            ++ PrintExp exp ++ ";\n"
        A.Typecast {var=v, typ=t}
            -> "(" ++ t ++ ")" ++ printVar v ++ ";"
        A.For -> "" -- not sure what to do, there are no for-loops in code
        A.Parens e -> "( " ++ printExp e ++ " )"
        A.Return s -> "return " ++ printExp s ++ ";"


    printVar :: A.Var -> String
    printVar var = v var ++
                  case idx var of
                    Just x -> "[" ++ x ++ "]"
                    Nothing -> ""

    printOp :: A.Op -> String
    printOp op = case op of
        Plus ->  "+"
        Minus -> "-"
        Times -> "*"
        And ->  "&&"
        Or -> "||"
        ExOr -> "^"
        InAnd -> "&"
        InOr -> "|"
        LShift -> "<<"
        RShift -> ">>"

    printArgs :: [A.Var] -> String
    printArgs [] = ""
    printArgs [x] = x
    printArgs (x:xs) = x ++ " ," ++ printArgs xs

    addIndents :: String -> String

