module Gen where

    import Ast as A

    -- Parameters imported from curve25519
    numWords = A.IntExp 10
    type32 = "crypto_int32"
    type64 = "crypto_int64"

    genAst :: String -> A.Prog
    genAst s =
        let p = genParams s
        in A.Prog [ genDefines,
                    genIncludes,
                    genZero,
                    genOne,
                    genAdd,
                    genSub,
                    genCopy,
                    genSwap,
                    genLoad3,
                    genLoad4
                    genFromBytes,
                    genToBytes,
                    genMul,
                    genSquare]

    genParams :: String -> Params
    genParams s = Params s

    genTypedef :: Params -> A.Dec
    genTypedef p = "crypto_int32" "fe[10]"

    genIncludes :: Params -> A.Dec
    genIncludes p = Include "fe.h"

    ---------------------
    -- Basic Functions --
    ---------------------
    -- duplicated exactly from supercpo ref10 implementation.

    genZero' :: A.IntExp -> A.Param -> [A.Assign]
    genZero' 0 _ = []
    genLoop n v' =
        let var' = A.Var { v=pvar v', idx=Show (n - 1) }
        in A.Assign { var=var', val=A.IntExp 0 }
           ++ genZero' (n - 1) v

    genZero :: A.Dec
    genZero =
        let param = A.Param { pvar="h",
                              ptyp=Just "fe" }
            body' = A.Seq ( genZero' numWords param )

        in A.FuncDec { name="fe_0",
                       params=[param],
                       result=Nothing,
                       body=body' }

    ----------------------------------------------------------------------------

    genOne' :: A.IntExp -> A.Param -> [A.Assign]
    genOne' 0 _ = []
    genOne' n v' = case n of
        1 -> let val' = A.IntExp 1
        _ -> let val' = A.IntExp 0
        in A.Assign { var=A.Var{ v=pvar v', idx=Show (n - 1) },
                      val=val' }
        ++ genOne' (n - 1) v

    genOne :: A.Dec
    genOne p =
        let param = A.Param { pvar="h",
                              ptyp=Just "fe" }
            body' = A.Seq (genOne' numWords param)

        in A.FuncDec { name="fe_1",
                       params=[param],
                       result=Nothing,
                       body=body' }

    ----------------------------------------------------------------------------

    genAssignFrom' :: A.IntExp -> A.Param -> A.Param -> [A.Assign]
    genAssignFrom' 0 _ _ = []
    genAssignFrom' n v1 v2 =
        let var' = A.Var {v=pvar v1 ++ Show (n - 1), idx=Nothing, typ=typ32 }
            val' = A.Var {v=pvar v2, idx=Show (n - 1)}
        in A.Assign { var=var',
                      val=val' }
           ++ genAssignTo' (n - 1) v1 v2


    genAssignTo' :: A.IntExp -> A.Param -> A.Param -> [A.Assign]
    genAssignTo' 0 _ _ = []
    genAssignTo' n v1 v2 =
        let var' = A.Var { v=pvar v1, idx = Show (n - 1) }
            val' = A.Var { v=pvar v2 ++ Show (n - 1), idx=Nothing }
        in A.Assign { var=var',
                      val=val' }
           ++ genAssignTo' (n - 1) v1 v2

    ----------------------------------------------------------------------------

    genAddSub' :: A.IntExp -> A.Param -> A.Param -> A.Op -> [A.Assign]
    genAddSub' 0 _ _ _ _  = []
    genAddSub' n v1 v2 v3 oper =
        let var' = A.Var { v=pvar v1 ++ Show n, idx=Nothing, typ=type32 }
            op' = A.OpExp { left=A.Var{ v=pvar v2 ++ Show n } ,
                            op=oper,
                            right=A.Var{ v=pvar v3++ Show n } }
        in A.Assign { var=var',
                      val=op' }
           ++ genAddSub' (n - 1) v1 v2 v3

    genAdd :: A.Dec
    genAdd =
        let h = A.Param { pvar="h",
                          ptyp=Just "fe"}
            f = A.Param { pvar="f",
                          ptyp=Just "fe"}
            g = A.Param { pvar="g",
                          ptyp=Just "fe"}

            body' = A.Seq [ genAssignFrom numWords f f,
                            genAssignFrom numWords g g,
                            genAddSub' numWords f g A.Plus,
                            genAssignTo numWords h h ]

        in A.FuncDec { name="fe_add",
                       params=[h, f, g],
                       rtype=Nothing,
                       body=body' }

    ----------------------------------------------------------------------------

    genSub :: A.Dec
    genSub =
        let h = A.Param { pvar="h",
                          ptyp="fe" }
            f = A.Param { pvar="f",
                          ptyp="fe" }
            g = A.Param { pvar="g",
                          ptyp="fe" }

            body' = A.Seq [ genAssignFrom numWords f f,
                            genAssignFrom numWords g g,
                            genAddSub' numWords f g A.Minus,
                            genAssignTo numWords h h ]

        in A.FuncDec { name="fe_sub",
                       params=[h, f, g],
                       rtype=Nothing,
                       body=body' }

    ----------------------------------------------------------------------------

    genCopy :: A.Dec
    genCopy =
        let f = A.Param { pvar="f",
                          ptyp="fe" }
            h = A.Param { pvar="h",
                          ptyp="fe" }

            body' = A.Seq [ genAssignFrom numWords f f,
                            genAssignTo nuumWords h f ]

        in A.FuncDec { name="fe_copy",
                       params=[h, f]
                       rtype=Nothing,
                       body=body'}

    ----------------------------------------------------------------------------

    genSwap :: A.Dec
    genSwap =
        let f = A.Param { pvar="f",
                          ptyp="fe" }
            g = A.Param { pvar="g",
                          ptyp="fe" }
            b = A.Param { pvar="b",
                          ptyp="unsigned int" }

            body' = ""

        in A.FuncDec { name="fe_cswap",
                       params=[f, g, b],
                       rtype=Nothing,
                       body=body' }

    ----------------------------------------------------------------------------

    genLoad3 :: A.Dec
    genLoad3 =
        let param = A.Param { pvar="in",
                              ptyp="const unsigned char" }
            body' = ""

        in A.FuncDec { name="load_3",
                       params= [param],
                       rtype="static crypto_uint64",
                       body=body' }

    ----------------------------------------------------------------------------

    genLoad4  :: A.Dec
    genLoad4 =
        let param = A.Param { pvar="in",
                              ptyp="const unsigned char" }
            body' = ""

        in A.FuncDec { name="load_3",
                       params= [param],
                       rtype="static crypto_uint64",
                       body=body' }
