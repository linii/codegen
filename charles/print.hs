-- pretty print module

module Print where

  import Absyn as A
  import Text.Regex

  printAbsyn :: A.Prog -> String
  printAbsyn x = "// This code was automatically generated by the " 
                 ++ "Crypto CodeGen Haskell utility\n\n"
                 ++ printAbsyn' x
  printAbsyn' (A.Prog []) = ""
  printAbsyn' (A.Prog (x:xs)) = printDec x ++ printAbsyn' (A.Prog xs)

  printDec :: A.Dec -> String
  printDec x = (
    case x of
      A.TypeDec {td=n, ty=t, size=m} -> 
        "type " ++ n ++ " [" ++ show m ++ "]" ++ t
      A.FunctionDec {fd=n, params=p, result=r, body=b} ->
        let result = case r of
                      Just n -> " " ++ n ++ " "
                      Nothing -> " "
        in ("func " ++ n ++ "(" ++ printParams p
              ++ ")" ++ result ++ "{\n  " 
              ++ addIndents (printExp b) ++ "\n}")
      A.PackageDec s -> "package " ++ s
      A.ImportDec s -> "import (\n  \"" ++ s ++ "\"\n)"
    ) ++ "\n\n"

  printParams :: [A.Param] -> String
  printParams [] = ""
  printParams [x] = printParam x
  printParams (x:xs) = printParam x ++ ", " ++ (printParams xs)

  printParam :: A.Param -> String
  printParam (A.Param {pvar=v, ptyp=t}) = v ++  case t of 
                                                  Just x -> " " ++ x 
                                                  Nothing -> ""

  printExp :: A.Exp -> String
  printExp e =
    case e of
      A.VarExp x -> printVar x
      A.MinusExp x -> "-" ++ printVar x
      A.IntExp x -> show x
      A.AppExp {func=f, args=a} -> f ++ "(" ++ printArgs a ++ ")"
      A.OpExp {left=l, oper=o, right=r} -> 
        printExp l ++ " " ++ printOp o ++ " " ++ printExp r
      A.SeqExp s ->
        case s of 
          []    -> ""
          [x]   -> printExp x
          x:xs  -> printExp x ++ "\n" ++ printExp (A.SeqExp xs)
      A.AssignExp {var=v, aexp=e, aoper=o} ->
        printVar v ++ " " ++ 
          (case o of 
            Just x -> printOp x 
            Nothing -> "") 
          ++ "= " ++ printExp e
      A.TypeCastExp {tcexp=e, tctyp=t} ->
        t ++ "(" ++ printExp e ++ ")"
      A.RangeExp {rvar=f, rangevar=r, rloop=b} ->
        "for " ++ v f ++ " := range " 
          ++ v r ++ " {\n  " ++ printExp b ++ "\n}"
      A.ForExp {fvar=f, lo=l, hi=h, finit=i, floop=b} -> ""
      A.VarDecExp {vd=vs, typ=t} -> "var " ++ printArgs vs ++ " " ++ t
      A.ParenExp a -> "(" ++ printExp a ++ ")"
      A.ReturnExp a -> "return " ++ printExp a
      A.NewLineExp -> ""

  printVar :: A.Var -> String
  printVar var =
    v var ++ case idx var of
              Just x -> "[" ++ x ++ "]"
              Nothing -> ""

  printOp :: A.Oper -> String
  printOp o =
    case o of
      PlusOp -> "+"
      MinusOp -> "-"
      TimesOp -> "*"
      AndOp -> "&"
      OrOp -> "|"
      ExOrOp -> "^"
      LShiftOp -> "<<"
      RShiftOp -> ">>"

  printArgs :: [A.Var] -> String
  printArgs [] = ""
  printArgs [v] = printVar v
  printArgs (v:vs) = printVar v ++ ", " ++ printArgs vs

  addIndents :: String -> String
  addIndents s = subRegex (mkRegex "\n") s "\n  "
