module Ast where
    data Prog = Prog [Dec]

    data Dec =
        FuncDec {name :: String, params :: [Param], rtype :: Maybe String, body :: Exp}
      | Typedef {type1 :: String, type2 :: String}
      | Ifdef {mode :: String}
      | Define {defname :: String, value :: String }
      | Include {file :: String}


    data Var = Var {v :: String, idx :: Maybe String, typ :: Maybe String}
    sVar = Var {v= "", idx=Nothing, typ=Nothing}

    data Param = Param { pvar :: String, ptyp :: String}

    data GenVar = VarX Var | ParamX Param

    data Exp =
        Seq [Exp]
      | VarExp (Var)
      | VarDec (Var)
      | IntExp (Int)
      | FuncApply {func :: String, args :: [Exp]}
      | OpExp {left :: Exp, oper :: Op, right :: Exp}
      | Negate (Var)
      | Assign {var :: Var, val :: Exp, op :: Maybe Op }
      | Typecast {tvar :: Exp, newtyp :: String}
      | For {from :: Exp, to :: Exp, loopvar :: Var, looptype :: String, incr :: Exp}
      | Parens (Exp)
      | Return (Exp)
      | Newline

    sAssign = Assign { var= sVar, val= VarExp sVar, op=Nothing }

    data Op =
        Plus
      | Minus
      | Times
      | And
      | Or
      | ExOr
      | LShift
      | RShift



