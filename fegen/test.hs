-- test module for the Crypto CodeGen Haskell utility
-- current only tests the opt module

module Test where

  import Absyn as A
  import Print as P
  import Opt as O
  import Data.List

  test :: String -> IO ()
  test s =
    let o   = if (isInfixOf "--unopt" s || isInfixOf "-u" s)
              then False
              else True
    in putStr (P.printAbsyn (genTests o))

  
  genTests :: Bool -> A.Prog
  genTests o = 
    A.Prog [ gen_prune1 o,
             gen_prune2 o,
             gen_prune3 o,
             gen_schedule1 o ]

  gen_prune1 :: Bool -> A.Dec
  gen_prune1 o = 
    let p   = []
        b   = A.InitExp { ivar=A.Var {v="unused", idx=Nothing},
                          iexp=A.IntExp(5) }
        b'  = O.prune o [[b]] p
    in A.FunctionDec  { fd="prune1",
                        params=p,
                        result=Nothing,
                        body=A.SeqExp b' }


  gen_prune2 :: Bool -> A.Dec
  gen_prune2 o =
    let p   = []
        v   = A.Var {v="unused1", idx=Nothing}
        b   = A.InitExp { ivar=v,
                          iexp=A.IntExp(5) }
        c   = A.InitExp { ivar=A.Var {v="unused2", idx=Nothing},
                          iexp=A.VarExp v }
        b'  = O.prune o [[b ,c]] p
    in A.FunctionDec  { fd="prune2",
                        params=p,
                        result=Nothing,
                        body=A.SeqExp b' }

  gen_prune3 :: Bool -> A.Dec
  gen_prune3 o =
    let p   = A.Param { pvar="unused2", ptyp=Just "*int" }
        v   = A.Var {v="unused1", idx=Nothing}
        b   = A.InitExp { ivar=v,
                          iexp=A.IntExp(5) }
        c   = A.InitExp { ivar=A.Var {v="unused2", idx=Nothing},
                          iexp=A.VarExp v }
        b'  = O.prune o [[b, c]] [p]
    in A.FunctionDec  { fd="prune3",
                        params=[p],
                        result=Nothing,
                        body=A.SeqExp b' }

  gen_schedule1 :: Bool -> A.Dec
  gen_schedule1 o = 
    let v1  = A.Var { v="v1", idx=Nothing }
        v2  = A.Var { v="v2", idx=Nothing }
        v3  = A.Var { v="v3", idx=Nothing }
        v4  = A.Var { v="v4", idx=Nothing }
        o1  = A.Var { v="o1", idx=Nothing }
        o2  = A.Var { v="o2", idx=Nothing }
        o3  = A.Var { v="o3", idx=Nothing }
        o4  = A.Var { v="o4", idx=Nothing }
        o5  = A.Var { v="o5", idx=Nothing }
        o6  = A.Var { v="o6", idx=Nothing }

        e1  = A.OpExp { left=A.VarExp v1,
                        oper=A.TimesOp,
                        right=A.VarExp v2 }
        e1' = A.InitExp { ivar=o1,
                          iexp=e1 }

        e2  = A.OpExp { left=A.VarExp v2,
                        oper=A.TimesOp,
                        right=A.VarExp v3 }
        e2' = A.InitExp { ivar=o2,
                          iexp=e2 }

        e3  = A.OpExp { left=A.VarExp v1,
                        oper=A.TimesOp,
                        right=A.VarExp v3 }
        e3' = A.InitExp { ivar=o3,
                          iexp=e3 }

        e4  = A.OpExp { left=A.VarExp v4,
                        oper=A.TimesOp,
                        right=A.VarExp v4 }
        e4' = A.InitExp { ivar=o4,
                          iexp=e4 }

        e5  = A.OpExp { left=A.VarExp v1,
                        oper=A.TimesOp,
                        right=A.VarExp v4 }
        e5' = A.InitExp { ivar=o5,
                          iexp=e5 }

        e6  = A.OpExp { left=A.VarExp v1,
                        oper=A.TimesOp,
                        right=A.VarExp v1 }
        e6' = A.InitExp { ivar=o5,
                          iexp=e6 }
        
        e   = O.schedule o [e1', e2', e3', e4', e5', e6']
    in A.FunctionDec  { fd="schedule1",
                        params=[],
                        result=Nothing,
                        body=A.SeqExp e }
