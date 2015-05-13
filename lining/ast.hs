module Ast where
    data Prog = Prog [Dec]

    data Dec =
        FuncDec {name :: String, params :: [Param], rtype :: Maybe String, body :: Exp}
      | Typedef {type1 :: String, type2 :: String}
      | Ifdef {mode :: String}
      | Define {defname :: String, value :: String }
      | Include {file :: String}


    data Var = Var {v :: String, idx :: Maybe String, typ :: Maybe String}
    data Param = Param { pvar :: String, ptyp :: String}

    data GenVar = VarX Var | ParamX Param

    data Exp =
        Seq [Exp]
      | VarExp (Var)
      | VarDec { vars :: [Var], vtyp :: String}
      | IntExp (Int)
      | FuncApply {func :: String, args :: [Var]}
      | OpExp {left :: Exp, oper :: Op, right :: Exp}
      | Negate (Var)
      | Assign {var :: Var, val :: Exp, op :: Maybe Op, atyp :: Maybe String}
      | Typecast {var :: Var, newtyp :: String}
      | For {from :: Exp, to :: Exp, loopvar :: Var, looptype :: String, incr :: Exp}
      | Parens (Exp)
      | Return (Exp)

    data Op =
        Plus
      | Minus
      | Times
      | And
      | Or
      | ExOr
      | LShift
      | RShift



