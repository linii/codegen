module Ast where
    data Prog = Prog [Dec]

    data Dec =
        Func {name :: String, params :: [Param], result :: Maybe String, rtype :: String, body :: Exp}
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
      | Op {left :: Exp, op :: Op, right :: Exp}
      | Assign {var :: Var, val :: Op, op :: Maybe Op}
      | Typecast {var :: Var, typ :: String}
      | For {from :: Exp, to :: Exp, loopvar :: Var, init :: Exp, incr :: Exp}
      | Return (Exp)

    data Op =
        Plus | Minus | Times | Negate
      | And | Or | ExOr | ExAnd
      | LShift | RShift


