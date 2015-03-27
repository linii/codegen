module Gen where

  import Absyn as A

  data Params = Params (String)

  genAbsyn :: String -> A.Prog
  genAbsyn s = 
    let p = genParams s
    in A.Prog [ gen_Package p,
                gen_Imports p,
                gen_Type p, 
                gen_feZero p, 
                gen_feOne p,
                gen_feAdd p,
                gen_feSub p,
                gen_feCopy p,
                gen_feCMove p,
                gen_load3 p,
                gen_load4 p ]

  genParams :: String -> Params
  genParams s = Params s

  gen_Package :: Params -> A.Dec
  gen_Package p = A.PackageDec "scalarmult"

  gen_Imports :: Params -> A.Dec
  gen_Imports p = A.ImportDec "fml"

  gen_Type :: Params -> A.Dec
  gen_Type p = A.TypeDec {td="fieldElement", ty="int32", size=10}

  gen_feZero :: Params -> A.Dec
  gen_feZero p = 
    let param = A.Param { pvar="fe",
                          ptyp=Just "*fieldElement" }
        exp'  = A.AssignExp { var=A.Var{v="fe", idx=Just "i"},
                              aexp=A.IntExp(0),
                              aoper=Nothing }
        body' = A.RangeExp  { rvar=A.Var{v="i", idx=Nothing},
                              rangevar=A.Var{v="fe", idx=Nothing},
                              rloop=exp' }

    in A.FunctionDec  { fd="feZero",
                        params=[param],
                        result=Nothing,
                        body=body' }

  gen_feOne :: Params -> A.Dec
  gen_feOne p = 
    let param = A.Param { pvar="fe",
                          ptyp = Just "*fieldElement" }
        exp1 = A.AppExp { func="feZero",
                          args=[A.Var{v="fe", idx=Nothing}]}
        exp2 = A.AssignExp {var=A.Var{v="fe", idx=Just "0"},
                            aexp=A.IntExp(1),
                            aoper=Nothing }
    in A.FunctionDec  { fd="feOne",
                        params=[param],
                        result=Nothing,
                        body=A.SeqExp [exp1, exp2] }

  gen_feAdd :: Params -> A.Dec
  gen_feAdd p =
    let param1 = A.Param { pvar="dst", ptyp=Nothing }
        param2 = A.Param { pvar="a", ptyp=Nothing }
        param3 = A.Param { pvar="b",
                          ptyp=Just "*fieldElement" }
        var1 = A.Var {v="dst", idx=Just "i"}
        var2 = A.Var {v="a", idx=Just "i"}
        var3 = A.Var {v="b", idx=Just "i"}
        exp1 = A.OpExp {left=A.VarExp(var2), 
                        oper=A.PlusOp, 
                        right=A.VarExp(var3)}
        exp2  = A.AssignExp { var=var1,
                              aexp=exp1,
                              aoper=Nothing }
        body' = A.RangeExp  { rvar=A.Var{v="i", idx=Nothing},
                              rangevar=A.Var{v="dst", idx=Nothing},
                              rloop=exp2 }

    in A.FunctionDec  { fd="feAdd",
                        params=[param1, param2, param3],
                        result=Nothing,
                        body=body' }

  gen_feSub :: Params -> A.Dec
  gen_feSub a =
    let param1 = A.Param { pvar="dst", ptyp=Nothing }
        param2 = A.Param { pvar="a", ptyp=Nothing }
        param3 = A.Param { pvar="b",
                          ptyp=Just "*fieldElement" }
        var1 = A.Var {v="dst", idx=Just "i"}
        var2 = A.Var {v="a", idx=Just "i"}
        var3 = A.Var {v="b", idx=Just "i"}
        exp1 = A.OpExp {left=A.VarExp(var2), 
                        oper=A.MinusOp, 
                        right=A.VarExp(var3)}
        exp2  = A.AssignExp { var=var1,
                              aexp=exp1,
                              aoper=Nothing }
        body' = A.RangeExp  { rvar=A.Var{v="i", idx=Nothing},
                              rangevar=A.Var{v="dst", idx=Nothing},
                              rloop=exp2 }

    in A.FunctionDec  { fd="feSub",
                        params=[param1, param2, param3],
                        result=Nothing,
                        body=body' }

  gen_feCopy :: Params -> A.Dec
  gen_feCopy p =
    let param1 = A.Param { pvar="dst", ptyp=Nothing }
        param2 = A.Param { pvar="src",
                           ptyp=Just "*fieldElement" }
        var1 = A.Var {v="dst", idx=Just "i"}
        var2 = A.Var {v="src", idx=Just "i"}
        exp'  = A.AssignExp { var=var1,
                              aexp=A.VarExp(var2),
                              aoper=Nothing }
        body' = A.RangeExp  { rvar=A.Var{v="i", idx=Nothing},
                              rangevar=A.Var{v="dst", idx=Nothing},
                              rloop=exp' }

    in A.FunctionDec  { fd="feCopy",
                        params=[param1, param2],
                        result=Nothing,
                        body=body' }

  gen_feCMove :: Params -> A.Dec
  gen_feCMove p =
    let param1 = A.Param { pvar="f", ptyp=Nothing }
        param2 = A.Param { pvar="g",
                           ptyp=Just "*fieldElement" }
        param3 = A.Param { pvar="b",
                           ptyp=Just "int32" }
        exp1 = A.VarDecExp {vd=[A.Var {v="x", idx=Nothing}],
                            typ="fieldElement"}
        exp2 = A.AssignExp {var=A.Var {v="b", idx=Nothing},
                            aexp = A.MinusExp(A.Var {v="b", idx=Nothing}),
                            aoper = Nothing }
        exp3 = A.ParenExp A.OpExp{left=A.VarExp (A.Var{v="f", idx=Just "i" }),
                                  oper=A.ExOrOp,
                                  right=A.VarExp (A.Var{v="g", idx= Just "i"})}
        exp4 = A.AssignExp {var=A.Var { v="x", idx = Just "i"},
                            aexp=
                              A.OpExp{left=A.VarExp(A.Var{v="b", idx=Nothing}),
                                      oper=A.AndOp,
                                      right=exp3},
                            aoper = Nothing }
        exp5 = A.RangeExp { rvar=A.Var{v="i", idx=Nothing},
                            rangevar=A.Var{v="x", idx=Nothing},
                            rloop=exp4 }
        exp6 = A.AssignExp {var=A.Var { v="f", idx=Just "i"},
                            aexp = A.VarExp (A.Var { v="x", idx=Just "i"}),
                            aoper = Just A.ExOrOp }
        exp7 = A.RangeExp { rvar=A.Var{v="i", idx=Nothing},
                            rangevar=A.Var{v="f", idx=Nothing},
                            rloop=exp6 }
    in A.FunctionDec  { fd="feCMove",
                        params=[param1, param2, param3],
                        result=Nothing,
                        body=A.SeqExp [exp1, exp2, exp5, exp7] }

  gen_load' :: Params -> [A.Exp]
  gen_load' p =
    let v = A.Var {v="r", idx=Nothing}
        exp1 = A.VarDecExp {vd=[v],
                            typ="int64"}
        exp2 = A.AssignExp {var=v,
                            aexp=A.TypeCastExp { 
                              tcvar=A.Var{ v="in", idx=Just "0"},
                              tctyp="int64" },
                            aoper = Nothing }
        exp3 = A.OpExp {left=A.TypeCastExp{ tcvar=A.Var{ v="in", idx=Just "1"},
                                            tctyp="int64" }, 
                        oper=A.LShiftOp,
                        right=A.IntExp(8)}
        exp4 = A.AssignExp {var=v,
                            aexp=exp3,
                            aoper=Just A.OrOp }
        exp5 = A.OpExp {left=A.TypeCastExp{ tcvar=A.Var{ v="in", idx=Just "2"},
                                            tctyp="int64" }, 
                        oper=A.LShiftOp,
                        right=A.IntExp(16)}
        exp6 = A.AssignExp {var=v,
                            aexp=exp5,
                            aoper=Just A.OrOp }
    in [exp1, exp2, exp4, exp6]

  gen_load3 :: Params -> A.Dec
  gen_load3 p =
    let exps = gen_load' p
        r = A.ReturnExp (A.VarExp A.Var {v="r", idx=Nothing})
        b = A.SeqExp (exps ++ [r])
    in A.FunctionDec  { fd="load3",
                        params=[A.Param{pvar="in", ptyp=Just "[]byte"}],
                        result=Just "int64",
                        body=b }

  gen_load4 :: Params -> A.Dec
  gen_load4 p =
    let v = A.Var {v="r", idx=Nothing}
        exps = gen_load' p
        exp1 = A.OpExp {left=A.TypeCastExp{ tcvar=A.Var{ v="in", idx=Just "3"},
                                            tctyp="int64" }, 
                        oper=A.LShiftOp,
                        right=A.IntExp(24)}
        exp2 = A.AssignExp {var=v,
                            aexp=exp1,
                            aoper=Just A.OrOp }
        r = A.ReturnExp (A.VarExp v)
        b = A.SeqExp (exps ++ [exp2, r])
    in A.FunctionDec  { fd="load4",
                        params=[A.Param{pvar="in", ptyp=Just "[]byte"}],
                        result=Just "int64",
                        body=b }




