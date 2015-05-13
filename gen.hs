module Gen where

    import Ast as A
    import Params as P

    type32 = "crypto_int32"
    type64 = "crypto_int64"

    genAst :: P.Params -> A.Prog
    genAst p =
        let n = len p
        in A.Prog [ genInclude1,
                    genInclude2,
                    genIfdef,
                    genTypedef n,
                    genZero n,
                    genOne n,
                    genAdd n,
                    genSub n,
                    genCopy n,
                    genSwap n,
                    genLoad3 n,
                    genLoad4 n,
                    genFromBytes p,
                    genToBytes p,
                    genMul p,
                    genSquare p
                    ]

    genInclude1 :: A.Dec
    genInclude1 = A.Include "fe.h"

    genInclude2 :: A.Dec
    genInclude2 = A.Include "crypto_int64.h"

    genIfdef :: A.Dec
    genIfdef = A.Ifdef "HAVE_TI_MODE"

    genTypedef :: Int -> A.Dec
    genTypedef n = A.Typedef "crypto_int32" ("fe[" ++ (show n) ++ "]")

    ---------------------
    -- Basic Functions --
    ---------------------
    -- duplicated exactly from supercpo ref10 implementation.

    genZero' :: Int -> A.Param -> [A.Exp]
    genZero' 0 _ = []
    genZero' n v' =
        let var' = A.Var { v=pvar v', idx=Just (show (n - 1)), typ=Nothing}
        in [ A.Assign { var=var',
                        val=A.IntExp 0,
                        op=Nothing,
                        atyp=Nothing } ]
           ++ genZero' (n - 1) v'

    genZero :: Int -> A.Dec
    genZero numWords =
        let param = A.Param { pvar="h",
                              ptyp="fe" }
            body' = A.Seq ( genZero' numWords param )

        in A.FuncDec { name="fe_0",
                       params=[param],
                       rtype=Nothing,
                       body=body' }

    ----------------------------------------------------------------------------

    genOne' :: Int -> A.Param -> [A.Exp]
    genOne' 0 _ = []
    genOne' n v' =
        let val' = case n of
                    1 -> A.IntExp 1
                    _ -> A.IntExp 0
        in [ A.Assign { var=A.Var { v=pvar v', idx=Just (show (n - 1)), typ=Nothing},
                        val=val',
                        op=Nothing,
                        atyp=Nothing } ]
        ++ genOne' (n - 1) v'

    genOne :: Int ->  A.Dec
    genOne numWords=
        let param = A.Param { pvar="h",
                              ptyp="fe" }
            body' = A.Seq (genOne' numWords param)

        in A.FuncDec { name="fe_1",
                       params=[param],
                       rtype=Nothing,
                       body=body' }

    ----------------------------------------------------------------------------


    genSimpleAssign' :: Int -> A.GenVar -> A.GenVar -> [A.Exp]
    genSimpleAssign' 0 _ _ = []
    genSimpleAssign' n v1 v2 =
        let var' = case v1 of
                    ParamX (A.Param {pvar=pv, ptyp=ptype}) ->
                        A.Var { v=pv, idx = Just (show (n - 1)), typ=Nothing}
                    VarX (A.Var {v=v1n, idx=i, typ=t}) ->
                        A.Var { v=v1n ++ show (n - 1), idx = Nothing, typ=t}
            val' = case v2 of
                    ParamX (A.Param {pvar=pv, ptyp=ptype}) ->
                        A.Var { v= pv, idx=Just (show (n - 1)), typ=Nothing}
                    VarX (A.Var {v=v2n, idx=i, typ=t}) ->
                        A.Var { v=v2n ++ show (n - 1), idx=Nothing, typ=Nothing}
        in [A.Assign { var=var',
                       val=A.VarExp (val'),
                       op=Nothing,
                       atyp=Nothing } ]
           ++ genSimpleAssign' (n - 1) v1 v2

    --------------------------------------------------------------------------

    genAdd :: Int -> A.Dec
    genAdd numWords =
        let h = A.Param { pvar="h",
                          ptyp="fe"}
            f = A.Param { pvar="f",
                          ptyp="fe"}
            g = A.Param { pvar="g",
                          ptyp="fe"}

            f' = A.Var { v="f", idx=Nothing, typ=Just type32 }
            g' = A.Var { v="g", idx=Nothing, typ=Just type32 }
            h' = A.Var { v="h", idx=Nothing, typ=Just type32 }

            body' = A.Seq ( genSimpleAssign' numWords (VarX f') (ParamX f) ++
                            genSimpleAssign' numWords (VarX g') (ParamX g) ++
                            genOper' numWords (VarX h') (ParamX f) (ParamX g) A.Plus ++
                            genSimpleAssign' numWords (ParamX h) (VarX h') )

        in A.FuncDec { name="fe_add",
                       params=[h, f, g],
                       rtype=Nothing,
                       body=body' }

    --------------------------------------------------------------------------

    genSub :: Int -> A.Dec
    genSub numWords =
        let h = A.Param { pvar="h",
                          ptyp="fe" }
            f = A.Param { pvar="f",
                          ptyp="fe" }
            g = A.Param { pvar="g",
                          ptyp="fe" }

            f' = A.Var { v="f", idx=Nothing, typ=Just type32 }
            g' = A.Var { v="g", idx=Nothing, typ=Just type32 }
            h' = A.Var { v="h", idx=Nothing, typ=Just type32 }

            body' = A.Seq ( genSimpleAssign' numWords (VarX f') (ParamX f) ++
                            genSimpleAssign' numWords (VarX g') (ParamX g)++
                            genOper' numWords (VarX h') (ParamX f) (ParamX g) A.Minus ++
                            genSimpleAssign' numWords (ParamX h) (VarX h') )

        in A.FuncDec { name="fe_sub",
                       params=[h, f, g],
                       rtype=Nothing,
                       body=body' }

    ----------------------------------------------------------------------------

    genCopy :: Int -> A.Dec
    genCopy numWords =
        let f = A.Param { pvar="f",
                          ptyp="fe" }
            h = A.Param { pvar="h",
                          ptyp="fe" }

            f' = A.Var { v="f", idx=Nothing, typ=Just type32 }

            body' = A.Seq ( genSimpleAssign' numWords (VarX f') (ParamX f) ++
                            genSimpleAssign' numWords (ParamX h) (VarX f') )

        in A.FuncDec { name="fe_copy",
                       params=[h, f],
                       rtype=Nothing,
                       body=body'}

    ----------------------------------------------------------------------------
    genAll' :: Int -> A.Var -> A.Param -> A.Op -> [A.Exp]
    genAll' 0 _ _ _ = []
    genAll' n v1 v2 op' =
        [ A.Assign {var=A.Var {v=(v v1) ++ (show (n - 1)), idx=Nothing, typ=Nothing},
                    val=A.VarExp (A.Var {v=pvar v2, idx=Nothing, typ=Nothing}),
                    op=Just op',
                    atyp=Nothing} ]
        ++ genAll' (n - 1) v1 v2 op'

    genSwap :: Int -> A.Dec
    genSwap numWords =
        let f = A.Param { pvar="f",
                          ptyp="fe" }
            g = A.Param { pvar="g",
                          ptyp="fe" }
            b = A.Param { pvar="b",
                          ptyp="unsigned int" }

            f' = A.Var { v="f", idx=Nothing, typ=Just type32 }
            g' = A.Var { v="g", idx=Nothing, typ=Just type32 }
            x = A.Var { v="x", idx=Nothing, typ=Just type32 }
            b' = A.Var {v= pvar b, idx=Nothing, typ=Nothing}

            body' = A.Seq ( genSimpleAssign' numWords (VarX f') (ParamX f) ++
                            genSimpleAssign' numWords (VarX g') (ParamX g) ++
                            genOper' numWords (VarX x) (VarX f') (VarX g') A.ExOr ++
                            [A.Assign {var=b',
                                      val=Negate b',
                                      op=Nothing,
                                      atyp=Nothing}] ++
                            genAll' numWords x b A.And ++
                            genOper' numWords (ParamX f) (VarX f') (VarX x) A.ExOr ++
                            genOper' numWords (ParamX g) (VarX g') (VarX x) A.ExOr
                            )

        in A.FuncDec { name="fe_cswap",
                       params=[f, g, b],
                       rtype=Nothing,
                       body=body' }

    ----------------------------------------------------------------------------

    genOper' :: Int -> A.GenVar -> A.GenVar -> A.GenVar -> A.Op -> [A.Exp]
    genOper' 0 _ _ _ _  = []
    genOper' n v1 v2 v3 oper' =
        let id = case v1 of
                    ParamX (A.Param {pvar=asdf, ptyp=paramtype}) -> Just (show (n - 1))
                    _ -> Nothing
            vtyp = case v1 of
                    VarX (A.Var {v=v1, idx=i, typ=t}) -> t
                    _ -> Nothing
            vname = case v1 of
                    ParamX (A.Param {pvar=v1name, ptyp=typ}) -> v1name
                    VarX (A.Var {v=v1n, idx=i, typ=t})-> v1n ++ show (n - 1)
            v2' = case v2 of
                    VarX (A.Var {v=v2name, typ=t}) -> v2name
                    ParamX (A.Param {pvar=v2n, ptyp=typ}) -> v2n
            v3' = case v3 of
                    VarX (A.Var {v=v3name, typ=t}) -> v3name
                    ParamX (A.Param {pvar=v3n, ptyp=typ}) -> v3n
        in [ A.Assign { var=A.Var { v=vname, idx=id, typ=vtyp},
                        val=A.OpExp { left=A.VarExp (A.Var{ v=v2' ++ show (n - 1),
                                                  idx=Nothing, typ=Nothing } ),
                                      oper=oper',
                                      right=A.VarExp (A.Var{ v=v3' ++ show (n - 1),
                                                             idx=Nothing, typ=Nothing } )},
                        op=Nothing,
                        atyp=Nothing } ]
           ++ genOper' (n - 1) v1 v2 v3 oper'

    ----------------------------------------------------------------------------

    genLoad3 :: Int -> A.Dec
    genLoad3 numWords =
        let param = A.Param { pvar="in",
                              ptyp="const unsigned char * " }
            body' = A.IntExp 0

        in A.FuncDec { name="load_3",
                       params= [param],
                       rtype=Just "static crypto_uint64",
                       body=body' }

    ----------------------------------------------------------------------------

    genLoad4 :: Int -> A.Dec
    genLoad4 numWords =
        let param = A.Param { pvar="in",
                              ptyp="const unsigned char * " }

            body' = A.IntExp 0

        in A.FuncDec { name="load_4",
                       params= [param],
                       rtype=Just "static crypto_uint64",
                       body=body' }

    ----------------------------------------------------------------------------

    genFromBytes :: P.Params -> A.Dec
    genFromBytes p =
        let h = A.Param { pvar="h",
                          ptyp="fe" }
            s = A.Param { pvar="s",
                          ptyp="const unsigned char *"}
            numWords = len p


            body' = A.Seq ( genSimpleAssign' numWords (ParamX h) (ParamX h) )

        in A.FuncDec { name="fe_frombytes",
                       params=[h, s],
                       rtype=Nothing,
                       body=body'}


    ----------------------------------------------------------------------------

    genToBytes :: P.Params -> A.Dec
    genToBytes p =
        let h = A.Param { pvar="h",
                          ptyp="fe" }
            s = A.Param { pvar="s",
                          ptyp="unsigned char *"}
            numWords = len p

            body' = A.Seq ( genSimpleAssign' numWords (ParamX h) (ParamX h) )

       in A.FuncDec { name="fe_tobytes",
                      params=[s, h],
                      rtype=Nothing,
                      body=body'}

    ----------------------------------------------------------------------------

    genMul ::  P.Params -> A.Dec
    genMul p =
        let h = A.Param { pvar="h",
                          ptyp="fe" }
            f = A.Param { pvar="f",
                          ptyp="fe" }
            g = A.Param { pvar="g",
                          ptyp="fe" }
            numWords = len p

            body' = A.Seq ( genSimpleAssign' numWords (ParamX f) (ParamX f) ++
                            genSimpleAssign' numWords (ParamX g) (ParamX g) ++
                            genSimpleAssign' numWords (ParamX h) (ParamX h ))

        in A.FuncDec { name="fe_mul",
                       params=[h, f, g],
                       rtype=Nothing,
                       body=body' }

    ----------------------------------------------------------------------------

    genSquare ::  P.Params -> A.Dec
    genSquare p =
        let h = A.Param { pvar="h", ptyp="fe" }
            f = A.Param { pvar="f", ptyp="fe" }

            numWords = len p

            body' = A.Seq  ( genSimpleAssign' numWords (ParamX f) (ParamX f) ++
                             genSimpleAssign' numWords (ParamX h) (ParamX h))

        in A.FuncDec { name="fe_sq",
                       params=[h, f ],
                       rtype=Nothing,
                       body=body' }

