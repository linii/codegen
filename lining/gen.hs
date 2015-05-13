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

    genAssignFrom' :: Int -> A.Param -> A.Param -> [A.Exp]
    genAssignFrom' 0 _ _ = []
    genAssignFrom' n v1 v2 =
        let var' = A.Var {v=pvar v1 ++ show (n - 1), idx=Nothing, typ=Just type32 }
            val' = A.Var {v=pvar v2, idx=Just (show (n - 1)), typ=Nothing }
        in [ A.Assign { var=var',
                        val=A.VarExp (val'),
                        op=Nothing,
                        atyp=Nothing } ]
           ++ genAssignTo' (n - 1) v1 v2


    genAssignTo' :: Int -> A.Param -> A.Param -> [A.Exp]
    genAssignTo' 0 _ _ = []
    genAssignTo' n v1 v2 =
        let var' = A.Var { v=pvar v1, idx = Just (show (n - 1)), typ=Nothing}
            val' = A.Var { v=pvar v2 ++ show (n - 1), idx=Nothing, typ=Nothing}
        in [A.Assign { var=var',
                       val=A.VarExp (val'),
                       op=Nothing,
                       atyp=Nothing } ]
           ++ genAssignTo' (n - 1) v1 v2

    ----------------------------------------------------------------------------

    genAddSub' :: Int -> A.Param -> A.Param -> A.Param -> A.Op -> [A.Exp]
    genAddSub' 0 _ _ _ _  = []
    genAddSub' n v1 v2 v3 oper' =
        let var' = A.Var { v=pvar v1 ++ show n, idx=Nothing, typ=Just type32 }
            op' = A.OpExp { left=A.VarExp (A.Var{ v=pvar v2 ++ show n,
                                                  idx=Nothing, typ=Nothing } ),
                            oper=oper',
                            right=A.VarExp (A.Var{ v=(pvar v3 )++ show n,
                                                   idx=Nothing, typ=Nothing } )}
        in [ A.Assign { var=var',
                        val=op' ,
                        op=Nothing,
                        atyp=Nothing } ]
           ++ genAddSub' (n - 1) v1 v2 v3 oper'

    genAdd :: Int -> A.Dec
    genAdd numWords =
        let h = A.Param { pvar="h",
                          ptyp="fe"}
            f = A.Param { pvar="f",
                          ptyp="fe"}
            g = A.Param { pvar="g",
                          ptyp="fe"}

            body' = A.Seq ( genAssignFrom' numWords f f ++
                            genAssignFrom' numWords g g ++
                            genAddSub' numWords h f g A.Plus ++
                            genAssignTo' numWords h h )

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

            body' = A.Seq ( genAssignFrom' numWords f f ++
                            genAssignFrom' numWords g g ++
                            genAddSub' numWords h f g A.Minus ++
                            genAssignTo' numWords h h )

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

            body' = A.Seq ( genAssignFrom' numWords f f ++
                            genAssignTo' numWords h f )

        in A.FuncDec { name="fe_copy",
                       params=[h, f],
                       rtype=Nothing,
                       body=body'}

    ----------------------------------------------------------------------------

    genSwap :: Int -> A.Dec
    genSwap numWords =
        let f = A.Param { pvar="f",
                          ptyp="fe" }
            g = A.Param { pvar="g",
                          ptyp="fe" }
            b = A.Param { pvar="b",
                          ptyp="unsigned int" }

            body' = A.IntExp 0

        in A.FuncDec { name="fe_cswap",
                       params=[f, g, b],
                       rtype=Nothing,
                       body=body' }

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


            body' = A.Seq ( genAssignTo' numWords h h )

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

            body' = A.Seq ( genAssignFrom' numWords h h )

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

            body' = A.Seq ( genAssignFrom' numWords f f ++
                            genAssignFrom' numWords g g ++
                            genAssignTo' numWords h h )

        in A.FuncDec { name="fe_mul",
                       params=[h, f, g],
                       rtype=Nothing,
                       body=body' }

    ----------------------------------------------------------------------------

    genSquare ::  P.Params -> A.Dec
    genSquare p =
        let h = A.Param { pvar="h",
                          ptyp="fe" }
            f = A.Param { pvar="f",
                          ptyp="fe" }
            numWords = len p

            body' = A.Seq  ( genAssignFrom' numWords f f ++
                             genAssignTo' numWords h h )

        in A.FuncDec { name="fe_sq",
                       params=[h, f],
                       rtype=Nothing,
                       body=body' }

