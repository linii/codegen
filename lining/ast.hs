module Ast where
    data Prog = Prog [Dec]

    data Dec =
        Func {name :: String, params :: [Param], rtype :: String, body :: Exp}
      | Typedef {type1 :: String, type2 :: String}
      | Define {name :: Var, val :: Exp }
      | Include {file :: String }

    data Var = Var {v :: String, idx :: Maybe String}

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
      | For {from :: Exp, to :: Exp, loopvar :: Var, init :: Exp, incr :: Exp}
      | Parens (Exp)
      | Return (Exp)

    data Op =
        Plus | Minus | Times
      | And | Or | ExOr | InAnd | InOr
      | LShift | RShift



