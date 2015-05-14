module Gen where

    import Ast as A
    import Params as P

    type32 = "crypto_int32"
    type64 = "crypto_int64"
    utype64 = "crypto_uint64"

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
                    genLoad3,
                    genLoad4,
                    genFromBytes p,
                    genToBytes p,
                    genMul p,
                    genSquare p,
                    genInvert p ]

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
        let param = A.Param { pvar="h", ptyp="fe" }
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
        let param = A.Param { pvar="h", ptyp="fe" }
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
        let h = A.Param { pvar="h", ptyp="fe"}
            f = A.Param { pvar="f", ptyp="fe"}
            g = A.Param { pvar="g", ptyp="fe"}

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
        let h = A.Param { pvar="h", ptyp="fe" }
            f = A.Param { pvar="f", ptyp="fe" }
            g = A.Param { pvar="g", ptyp="fe" }

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
        let f = A.Param { pvar="f", ptyp="fe" }
            h = A.Param { pvar="h", ptyp="fe" }

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
        let f = A.Param { pvar="f", ptyp="fe" }
            g = A.Param { pvar="g", ptyp="fe" }
            b = A.Param { pvar="b", ptyp="unsigned int" }

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

    genLoad' :: Int -> Int -> A.Var -> A.Param -> [A.Exp]
    genLoad' 0 0 v1 v2 =
        let var' = A.Var{v=pvar v2, idx=Just (show 0), typ=Nothing}
            val' = A.Typecast{tvar=A.VarExp var' ,
                              newtyp=utype64}
        in [A.Assign{var=v1, val=val', op=Nothing, atyp=Nothing} ]

    genLoad' n x v1 v2 =
        let var' = A.Var{v=pvar v2, idx=Just (show x), typ=Nothing }
            val' = Parens ( A.Typecast { tvar=A.VarExp var' ,
                                         newtyp=utype64})
        in [A.Assign {var=v1,
                      val= A.OpExp{ left=val', right= A.IntExp n, oper=A.LShift },
                      op=Just A.Or,
                      atyp=Nothing} ]
            ++ genLoad' (n - 8) (x - 1) v1 v2

    genLoad3 :: A.Dec
    genLoad3 =
        let param = A.Param { pvar="in", ptyp="const unsigned char *" }
            result = A.Var {v="result", idx=Nothing, typ=Just utype64}
            main = genLoad' 16 2 result param
            return = A.Return (A.VarDec result)
            body' = A.Seq ([A.VarDec result] ++ main ++ [return])

        in A.FuncDec { name="load_3",
                       params= [param],
                       rtype=Just "static crypto_uint64",
                       body=body' }

    genLoad4 :: A.Dec
    genLoad4 =
        let param = A.Param { pvar="in", ptyp="const unsigned char *" }
            result = A.Var {v="result", idx=Nothing, typ=Just utype64}
            main = genLoad' 24 3 result param
            return = A.Return (A.VarDec result)
            body' = A.Seq ([A.VarDec result] ++ main ++ [return])

        in A.FuncDec { name="load_4",
                       params= [param],
                       rtype=Just "static crypto_uint64",
                       body=body'}

    ----------------------------------------------------------------------------

    genVarDecs' :: Int -> Var -> [A.Exp]
    genVarDecs' 0 _ = []
    genVarDecs' n v1 = [A.VarDec (A.Var {v= (v v1) ++ (show (n - 1)),
                                         typ=(typ v1),
                                         idx=Nothing})]
                        ++ genVarDecs' (n - 1) v1


    -- arguments:
    -- load pattern
    -- cumulative representation/size pattern from param for curve25519 (fixed)
    -- counter for variable
    -- for 32-bit word representation
    genFromBytesLoad' :: [Int] -> [Int] -> Int -> Int -> String -> [A.Exp]
    genFromBytesLoad' [] _ _ _ _ = []
    genFromBytesLoad' (x:xs) (y:ys) n cumu name =
        let -- left side of assignment exprssion
            var' = A.Var { v=name++show n, idx=Nothing, typ=Just type64}

            -- what you apply load function to; s variable
            fvar' = A.Var { v="s", idx=Nothing, typ=Nothing}
            arg' = A.OpExp { left=A.VarExp fvar', right=A.IntExp cumu, oper=A.Plus}

            -- load function application expression; left half of right side
            app' = A.FuncApply { func="load_"++ show x, args= [arg'] }

            app'' = case xs of  -- because sometimes you have do weird stuff
                    [] -> A.Parens A.OpExp { left=app',
                                             oper=A.And,
                                             right=A.IntExp(2^(x*8 - 1) - 1)}
                    _ -> app'

            remainder = 8 * cumu - y  -- how many bits are left over
            val' = case remainder of  -- right side of assignment expression
                    0 -> app''
                    _ -> A.OpExp { left=app'',  -- shift by number of leftover bits
                                   right=A.IntExp remainder,
                                   oper=A.LShift}

        -- full assignment expression
        in [A.Assign { var=var', val=val', op=Nothing, atyp=Nothing}]
            ++ genFromBytesLoad' xs ys (n + 1) (cumu + x) name


    genFromBytesCarries' :: [Int] -> [Int] -> Int -> [A.Exp]
    genFromBytesCarries' [] [] _ = []
    genFromBytesCarries' (x:xs) (y:ys) offset =
        let var' = A.Var{ v="carry" ++ show x, idx=Nothing, typ=Nothing}
            hvar' = case offset of
                0 -> A.Var{ v="h" ++ (show (x+1)), idx=Nothing, typ=Nothing}
                _ -> A.Var{ v="h0", idx=Nothing, typ=Nothing}
            hvar'' = A.Var{v="h" ++ show x, idx=Nothing, typ=Nothing}

            shiftExp = A.OpExp {left=A.IntExp 1,
                                right=A.IntExp (y - 1),
                                oper=A.LShift}
            val'' = A.OpExp { left=A.VarExp hvar'',
                              right=A.Typecast { tvar=A.Parens shiftExp,
                                                 newtyp=type64 },
                              oper=A.Plus }

            s1val = A.OpExp  { left=val'',
                              right=A.IntExp y,
                              oper=A.RShift }
            s2val = case offset of
                0 -> A.VarExp var'
                _ ->  A.OpExp {left=A.VarExp hvar'',
                      oper=A.Times,
                      right=A.IntExp offset }
            s3val = A.OpExp { left=A.VarExp var',
                              right=A.IntExp y,
                              oper=A.LShift }

            s1 = A.Assign { var=var', val=s1val, op=Nothing, atyp=Nothing }
            s2 = A.Assign { var=hvar', val=s2val, op=Just A.Plus, atyp=Nothing }
            s3 = A.Assign { var=hvar'', val=s3val, op=Just A.Minus, atyp=Nothing }

        in [s1, s2, s3] ++ [A.Newline] ++ genFromBytesCarries' xs ys 0
    genFromBytesCarries' _ _ _ = error ("oh noes, incorrect format")

    --- small helper function for extracting odd, even elements of lists
    oddEvens :: [Int] -> ([Int], [Int])
    oddEvens [] = ([], [])
    oddEvens [x] = ([x], [])
    oddEvens (x:y:xs) = (x:xp, y:yp) where (xp, yp) = oddEvens xs


    genFromBytes :: P.Params -> A.Dec
    genFromBytes p =
        let rep' = rep p
            l = len p

            h = A.Param { pvar="h", ptyp="fe" }
            s = A.Param { pvar="s", ptyp="const unsigned char *"}
            loadpattern = [4, 3, 3, 3, 3, 4, 3, 3, 3, 3 ] -- how the heck did they get this
            cumulative = scanl1 (+) (0:(rep p))

            reps = drop (l - 1) (rep' ++ rep')
            indices = drop (l - 1) ([0.. l - 1] ++ [0..l - 1])
            oddreps =  init (fst (oddEvens reps))
            evenreps = snd (oddEvens reps)
            oddi = init (fst (oddEvens indices))
            eveni = snd (oddEvens indices)

            carry = A.Var {v="carry", idx=Nothing, typ=Just type64}

            load' = genFromBytesLoad' loadpattern cumulative 0 0 (pvar h)
            decs' = genVarDecs' l carry
            carryOdds' = genFromBytesCarries' oddi oddreps (offset p)
            carryEvens' = genFromBytesCarries' eveni evenreps 0
            assign' = genSimpleAssign' l (ParamX h) (ParamX h)

            body' = A.Seq ( load' ++ [A.Newline]
                            ++ decs' ++ [A.Newline]
                            ++ carryOdds' ++ [A.Newline]
                            ++ carryEvens' ++ [A.Newline]
                            ++ assign')

        in A.FuncDec { name="fe_frombytes",
                       params=[h, s],
                       rtype=Nothing,
                       body=body'}

    ----------------------------------------------------------------------------

    genToBytesPlace' :: Int -> [Int] -> A.Var -> A.Param -> [A.Exp]
    genToBytesPlace' _ [] _ _ = []
    genToBytesPlace' n (x:xs) v1 v2 =
        let var' = A.Var { v=v v1 , idx=Nothing, typ=Nothing }
            var'' = A.Var { v=pvar v2 ++ show n, idx=Nothing, typ=Nothing}
            l = Parens A.OpExp { left=A.VarExp var'',
                                 right=A.VarExp var',
                                 oper=A.Plus }
            r = A.OpExp { left=l,
                          right=A.IntExp x,
                          oper=A.RShift }

        in  [A.Assign { var= A.Var{v=v v1, idx=Nothing, typ=Nothing },
                        val=r, op=Nothing, atyp=Nothing}]
            ++ genToBytesPlace' (n + 1) xs v1 v2


    genToBytesCarry' :: Int -> [Int] -> [A.Exp]
    genToBytesCarry' _ [] = []
    genToBytesCarry' n (x:xs) =
        let var' = A.Var { v="carry" ++ show n, typ=Nothing, idx=Nothing }
            h' = A.Var { v="h" ++ show n, typ=Nothing, idx=Nothing }
            h'' = A.Var { v="h" ++ show (n + 1), typ=Nothing, idx=Nothing }

            s1val = A.OpExp { left=A.VarExp h', right=A.IntExp x, oper=A.RShift }
            s3val = A.OpExp { left=A.VarExp var', right=A.IntExp x, oper=A.LShift }

            s1 = A.Assign { var=var', val= s1val, op=Nothing, atyp=Nothing }
            s2 = case n of
                9 -> A.Newline
                _ -> A.Assign { var=h'',
                                val = A.VarExp var',
                                op=Just A.Plus,
                                atyp=Nothing }

            s3 = A.Assign { var=h',
                            val=s3val,
                            op=Just A.Minus,
                            atyp=Nothing }

        in [s1, s2, s3] ++ [A.Newline] ++ genToBytesCarry' (n + 1) xs


    genToBytesMod' :: [Int] -> [Int] -> [Int] -> Int -> Int -> [A.Exp]
    genToBytesMod' [] [] _ _ _ = []
    genToBytesMod' (x:xs) (y:ys) (z:zs) n1 n2 =
        let var' = A.VarExp A.Var { v="h" ++ show n1, idx=Nothing, typ=Nothing }

            x1 = A.Var { v="s", idx=Just (show n2), typ=Nothing }
            x2 = A.Var { v="s", idx=Just (show (n2 + 1)), typ=Nothing }
            x3 = A.Var { v="s", idx=Just (show (n2 + 2)), typ=Nothing }
            x4 = case x of
                    4 -> A.Var { v="s", idx=Just (show (n2 + 3)), typ=Nothing }
                    _ -> A.Var { v="", idx=Nothing, typ=Nothing }

            val' = 8 * n2 - z  -- how many bits are left over
            a1 = A.OpExp { left=var',
                           right=A.IntExp val',
                           oper=A.RShift}

            a2 = A.OpExp { left=var',
                           right=A.IntExp (val' + 8),
                           oper=A.RShift}

            var'' = A.Var { v="h" ++ show (n1 + 1), idx=Nothing, typ=Nothing }

            a3base = A.OpExp { left=var',
                               right=A.IntExp (val' + 16),
                               oper=A.RShift }

            leftover = case x of
                4 -> y - (val' + 24)
                _ -> y - (val' + 16)


            a' = A.OpExp { left=A.VarExp var'',
                           right=A.IntExp leftover,
                           oper=A.LShift }

            final = case leftover of
                8 -> a3base
                _ -> A.OpExp { left=Parens a3base, right=a', oper=A.Or }

            a3 = case x of
                    3 -> final
                    _ -> a3base

            a4' = A.OpExp { left=var',
                            right=A.IntExp (val' + 24),
                            oper=A.RShift }

            a4 = case x of
                    4 -> A.OpExp { left=a4', right=a', oper=A.Or }
                    _ -> A.Newline

            s1 = A.Assign { var=x1, val=a1, op=Nothing, atyp=Nothing}
            s2 = A.Assign { var=x2, val=a2, op=Nothing, atyp=Nothing}
            s3 = A.Assign { var=x3, val=a3, op=Nothing, atyp=Nothing}
            s4 = case x of
                    4-> A.Assign { var=x4, val=a4, op=Nothing, atyp=Nothing}
                    _-> A.Newline

        in [s1, s2, s3, s4] ++ [A.Newline] ++ genToBytesMod' xs ys zs (n1 + 1) (n2 + x)


    genToBytes :: P.Params -> A.Dec
    genToBytes p =
        let h = A.Param { pvar="h", ptyp="fe" }
            s = A.Param { pvar="s", ptyp="unsigned char *"}
            carry = A.Var {v="carry", idx=Nothing, typ=Just type32}
            q = A.Var{v="q", idx=Nothing, typ=Just type32}
            numWords = len p
            rep' = rep p
            o = offset p
            loadpattern = [4, 3, 3, 3, 3, 4, 3, 3, 3, 3]
            cumulative = scanl1 (+) (0:(rep p))
            lastvar' = A.Var { v="h"++show (numWords - 1), idx=Nothing, typ=Nothing }

            mod1 = A.OpExp { left=A.IntExp o,
                             right=A.VarExp lastvar',
                             oper=A.Times }

            cast' = A.Typecast { tvar=A.IntExp 1,
                                 newtyp=type32}
            mod2 = A.OpExp { left = Parens cast',
                             right=A.IntExp ((last rep' ) - 1),
                             oper=A.LShift }

            mod = A.OpExp { left=mod1,
                            right= Parens mod2,
                            oper=A.Plus }

            wrap = A.Assign { var = A.Var{ v=v q, idx=Nothing, typ=Nothing },
                              val = A.OpExp { left=mod,
                                              right=A.IntExp (last rep'),
                                              oper=A.RShift },
                              op=Nothing, atyp=Nothing }

            body' = A.Seq ( genSimpleAssign' numWords (ParamX h) (ParamX h) ++
                            [A.VarDec q ] ++
                            genVarDecs' numWords carry ++ [A.Newline] ++
                            [wrap] ++
                            genToBytesPlace' 0 rep' q h ++ [A.Newline] ++
                            genToBytesCarry' 0 rep' ++
                            genToBytesMod' loadpattern rep' cumulative 0 0)

       in A.FuncDec { name="fe_tobytes",
                      params=[s, h],
                      rtype=Nothing,
                      body=body'}

    ----------------------------------------------------------------------------

    genMul ::  P.Params -> A.Dec
    genMul p =
        let h = A.Param { pvar="h", ptyp="fe" }
            f = A.Param { pvar="f", ptyp="fe" }
            g = A.Param { pvar="g", ptyp="fe" }
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
            carry = A.Var {v="carry", idx=Nothing, typ=Just type32}
            numWords = len p

            body' = A.Seq  ( genSimpleAssign' numWords (ParamX f) (ParamX f) ++
                             genSimpleAssign' numWords (ParamX h) (ParamX h) ++
                             genVarDecs' numWords carry)

        in A.FuncDec { name="fe_sq",
                       params=[h, f ],
                       rtype=Nothing,
                       body=body' }


    ----------------------------------------------------------------------------
    -- https://github.com/jedisct1/libsodium/blob/master/src/libsodium/crypto_scalarmult/curve25519/ref10/fe_invert_curve25519_ref10.c
    -- there seems to be some things missing...

    genInvert :: P.Params -> A.Dec
    genInvert p =
        let out = A.Param { pvar="out", ptyp="fe"}
            z = A.Param {pvar="z", ptyp="fe"}

            body' = A.IntExp 0

        in A.FuncDec { name="fe_invert",
                       params=[out, z],
                       rtype=Nothing,
                       body=body' }

