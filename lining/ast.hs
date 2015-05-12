module Ast where
    data Prog = Prog [Dec]

    data Dec =
        FuncDec {name :: String, params :: [Param], rtype :: String, body :: Exp}
      | Typedef {type1 :: String, type2 :: String}
      | Define {name :: Var, val :: Exp }
      | Include {file :: String }

    data Var = Var {v :: String, idx :: Maybe String, typ :: Maybe String}

    data Param = Param { pvar :: String, ptyp :: String}

    data Exp =
      | Seq [Exp]
      | Var (Var)
      | VarDec { vars :: [Var], typ :: String}
      | IntExp (Int)
      | FuncApply {func :: String, args :: [Var]}
      | OpExp {left :: Exp, op :: Op, right :: Exp}
      | Negate (Var)
      | Assign {var :: Var, val :: Exp, op :: Maybe Op}
      | Typecast {var :: Var, typ :: String}
      | For {from :: Exp, to :: Exp, loopvar :: Var, looptype :: String, incr :: Exp}
      | Parens (Exp)
      | Return (Exp)

    data Op =
        Plus | Minus | Times
      | And | Or | ExOr
      | LShift | RShift



