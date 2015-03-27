module Absyn where

  data Prog = Prog [Dec]

  data Var = Var {v :: String, idx :: Maybe String}

  data Exp
     = VarExp (Var)
     | MinusExp (Var)
     | IntExp (Int)
     | AppExp {func :: String, args :: [Var]}
     | OpExp {left :: Exp, oper :: Oper, right :: Exp}
     | SeqExp [Exp]
     | AssignExp {var :: Var, aexp :: Exp, aoper :: Maybe Oper}
     | TypeCastExp {tcvar :: Var, tctyp :: String}
     | RangeExp {rvar :: Var, rangevar :: Var, rloop :: Exp}
     | ForExp {fvar :: Var, lo :: Exp, hi :: Exp, finit :: Exp, floop :: Exp}
     | VarDecExp {vd :: [Var], typ :: String}
     | ParenExp (Exp)
     | ReturnExp (Exp)

  data Dec
     = FunctionDec {fd :: String, params :: [Param], result :: Maybe String, body :: Exp} 
    -- TypeDec only used once, e.g. fieldElement [10]int32
     | TypeDec {td :: String, ty :: String, size :: Int}
     | ImportDec {id :: String}
     | PackageDec {pd :: String}

  
  data Oper = PlusOp | MinusOp | TimesOp
            | AndOp | OrOp | ExOrOp | LShiftOp | RShiftOp

  data Param = Param {pvar :: String, ptyp :: Maybe String}