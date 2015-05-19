module Gen where
    import Data.List

    import Ast as A
    import Params as P

    type32  = "crypto_int32"
    type64  = "crypto_int64"
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
        let var'   = A.sVar { v=pvar v', idx=Just (show (n - 1)) }
            assign = A.sAssign { var=var', val=A.IntExp 0 }
        in genZero' (n - 1) v' ++ [assign]

    genZero :: Int -> A.Dec
    genZero numWords =
        let param = A.Param { pvar="h", ptyp="fe" }
            body' = A.Seq ( genZero' numWords param )

        in A.FuncDec { name   = "fe_0",
                       params = [param],
                       rtype  = Nothing,
                       body   = body' }

    ----------------------------------------------------------------------------

    genOne' :: Int -> A.Param -> [A.Exp]
    genOne' 0 _ = []
    genOne' n v' =
        let val'   = case n of
                        1 -> A.IntExp 1
                        _ -> A.IntExp 0
            var'   = A.sVar { v=pvar v', idx=Just (show (n - 1)) }
            assign = A.sAssign { var=var', val=val' }
        in genOne' (n - 1) v' ++ [assign]

    genOne :: Int ->  A.Dec
    genOne numWords=
        let param = A.Param { pvar="h", ptyp="fe" }
            body' = A.Seq ( genOne' numWords param )

        in A.FuncDec { name   = "fe_1",
                       params = [param],
                       rtype  = Nothing,
                       body   = body' }

    ----------------------------------------------------------------------------

    genAdd :: Int -> A.Dec
    genAdd numWords =
        let h     = A.Param { pvar="h", ptyp="fe" }
            f     = A.Param { pvar="f", ptyp="fe" }
            g     = A.Param { pvar="g", ptyp="fe" }

            f'    = A.Var { v="f", idx=Nothing, typ=Just type32 }
            g'    = A.Var { v="g", idx=Nothing, typ=Just type32 }
            h'    = A.Var { v="h", idx=Nothing, typ=Just type32 }

            s1    = genSimpleAssign numWords (VarX f') (ParamX f)
            s2    = genSimpleAssign numWords (VarX g') (ParamX g)
            s3    = genOper numWords (VarX h') (ParamX f) (ParamX g) A.Plus
            s4    = genSimpleAssign numWords (ParamX h) (VarX h')

            body' = A.Seq ( s1 ++ [A.Newline] ++ s2 ++ [A.Newline] ++
                            s3 ++ [A.Newline] ++ s4 )

        in A.FuncDec { name   = "fe_add",
                       params = [h, f, g],
                       rtype  = Nothing,
                       body   = body' }

    --------------------------------------------------------------------------

    genSub :: Int -> A.Dec
    genSub numWords =
        let h     = A.Param { pvar="h", ptyp="fe" }
            f     = A.Param { pvar="f", ptyp="fe" }
            g     = A.Param { pvar="g", ptyp="fe" }

            f'    = A.Var { v="f", idx=Nothing, typ=Just type32 }
            g'    = A.Var { v="g", idx=Nothing, typ=Just type32 }
            h'    = A.Var { v="h", idx=Nothing, typ=Just type32 }

            s1    = genSimpleAssign numWords (VarX f') (ParamX f)
            s2    = genSimpleAssign numWords (VarX g') (ParamX g)
            s3    = genOper numWords (VarX h') (ParamX f) (ParamX g) A.Minus
            s4    = genSimpleAssign numWords (ParamX h) (VarX h')

            body' = A.Seq ( s1 ++ [A.Newline] ++ s2 ++ [A.Newline] ++
                            s3 ++ [A.Newline] ++ s4 )

        in A.FuncDec { name   = "fe_sub",
                       params = [h, f, g],
                       rtype  = Nothing,
                       body   = body' }

    ----------------------------------------------------------------------------

    genCopy :: Int -> A.Dec
    genCopy numWords =
        let f     = A.Param { pvar="f", ptyp="fe" }
            h     = A.Param { pvar="h", ptyp="fe" }

            f'    = A.Var { v="f", idx=Nothing, typ=Just type32 }

            s1    = genSimpleAssign numWords (VarX f') (ParamX f)
            s2    = genSimpleAssign numWords (ParamX h) (VarX f')

            body' = A.Seq ( s1 ++ [A.Newline] ++ s2 )

        in A.FuncDec { name   = "fe_copy",
                       params = [h, f],
                       rtype  = Nothing,
                       body   = body'}

    ----------------------------------------------------------------------------

    genSwap :: Int -> A.Dec
    genSwap numWords =
        let f     = A.Param { pvar="f", ptyp="fe" }
            g     = A.Param { pvar="g", ptyp="fe" }
            b     = A.Param { pvar="b", ptyp="unsigned int" }

            f'    = A.Var { v="f", idx=Nothing, typ=Just type32 }
            g'    = A.Var { v="g", idx=Nothing, typ=Just type32 }
            x     = A.Var { v="x", idx=Nothing, typ=Just type32 }
            b'    = A.sVar { v= pvar b }

            s1    = genSimpleAssign numWords (VarX f') (ParamX f)
            s2    = genSimpleAssign numWords (VarX g') (ParamX g)
            s3    = genOper numWords (VarX x) (VarX f') (VarX g') A.ExOr
            s4    = A.sAssign { var=b', val=Negate b' }

            s5    = genAll' numWords x b A.And
            s6    = genOper numWords (ParamX f) (VarX f') (VarX x) A.ExOr
            s7    = genOper numWords (ParamX g) (VarX g') (VarX x) A.ExOr

            body' = A.Seq ( s1 ++ [A.Newline] ++ s2 ++ [A.Newline] ++
                            s3 ++ [A.Newline] ++ [s4] ++ [A.Newline] ++
                            s5 ++ [A.Newline] ++ s6 ++ [A.Newline] ++
                            s7 )

        in A.FuncDec { name   = "fe_cswap",
                       params = [f, g, b],
                       rtype  = Nothing,
                       body   = body' }

    ----------------------------------------------------------------------------

    genLoad' :: Int -> Int -> A.Var -> A.Param -> [A.Exp]
    genLoad' n x v1 v2 =
        let var'         = A.sVar { v=pvar v2, idx=Just (show x) }
            val'         = A.Typecast { tvar=A.VarExp var', newtyp=utype64 }
            (opval, op') = case n of
                             0 -> (val', Nothing)
                             _ -> ( A.OpExp { left=Parens val',
                                              right= A.IntExp n,
                                              oper=A.LShift },
                                    Just A.Or )
            assign       = A.Assign { var=v1, val=opval, op=op' }

        in case n of
                0 -> [assign]
                _ -> genLoad' (n - 8) (x - 1) v1 v2 ++ [assign]

    genLoad3 :: A.Dec
    genLoad3 =
        let param  = A.Param { pvar="in", ptyp="const unsigned char *" }
            result = A.Var { v="result", idx=Nothing, typ=Just utype64 }

            s1     = A.VarDec result
            s2     = genLoad' 16 2 result param
            s3     = A.Return (A.VarDec result)

            body'  = A.Seq ( [s1] ++ s2 ++ [s3] )

        in A.FuncDec { name   ="load_3",
                       params = [param],
                       rtype  = Just "static crypto_uint64",
                       body   = body' }

    genLoad4 :: A.Dec
    genLoad4 =
        let param  = A.Param { pvar="in", ptyp="const unsigned char *" }
            result = A.Var { v="result", idx=Nothing, typ=Just utype64 }

            s1     = A.VarDec result
            s2     = genLoad' 24 3 result param
            s3     = A.Return (A.VarDec result)

            body'  = A.Seq ( [s1] ++ s2 ++ [s3] )

        in A.FuncDec { name   = "load_4",
                       params = [param],
                       rtype  = Just "static crypto_uint64",
                       body   = body' }

    ----------------------------------------------------------------------------
    -------------------------------FROM_BYTES-----------------------------------
    ----------------------------------------------------------------------------

    -- (x:xs) : load pattern
    -- (y:ys) : cumulative representation/size pattern from param for curve25519
    -- n      : linear counter for variable name
    -- cumu   : counter for how many bytes have been loaded from the source word
    -- name   : name of variable
    genFromBytesLoad' :: [Int] -> [Int] -> Int -> Int -> String -> [A.Exp]
    genFromBytesLoad' [] _ _ _ _ = []
    genFromBytesLoad' (x:xs) (y:ys) n cumu name =
        let var'  = A.sVar { v=name++show n, typ=Just type64 }

            fvar' = A.sVar { v="s" }
            arg'  = A.OpExp { left=A.VarExp fvar',
                              right=A.IntExp cumu,
                              oper=A.Plus }

            -- load function application expression; left half of right side
            app'  = A.FuncApply { func="load_"++ show x, args= [arg'] }

            app'' = case xs of  -- because sometimes you have do weird stuff
                        [] -> A.Parens A.OpExp { left=app',
                                                 oper=A.And,
                                                 right=A.IntExp(2^(x*8-1) - 1) }
                        _ -> app'

            rem   = 8 * cumu - y  -- how many bits are left over
            val'  = case rem of  -- right side of assignment expression
                        0 -> app''
                        _ -> A.OpExp { left=app'', --shift by # of leftover bits
                                       right=A.IntExp rem,
                                       oper=A.LShift }

            assign = A.sAssign { var=var', val=val' }
        in [assign] ++ genFromBytesLoad' xs ys (n + 1) (cumu + x) name


    -- (x:xs)   : index of carry variable; odds, then evens
    -- (y:ys)   : corresponding representation word size at those indices
    -- offset   : offset of curve as determined in params; c, where P = 2^m - c
    genFromBytesCarries' :: [Int] -> [Int] -> Int -> [A.Exp]
    genFromBytesCarries' [] [] _ = []
    genFromBytesCarries' (x:xs) (y:ys) offset =
        let var'   = A.sVar { v="carry" ++ show x }
            hvar'  = case offset of
                            0 -> A.sVar { v="h" ++ (show (x + 1)) }
                            _ -> A.sVar { v="h0" }

            hvar'' = A.sVar { v="h" ++ show x }

            shift  = A.OpExp { left=A.IntExp 1,
                               right=A.IntExp (y - 1),
                               oper=A.LShift}

            val''  = A.OpExp { left=A.VarExp hvar'',
                               right=A.Typecast { tvar=A.Parens shift,
                                                  newtyp=type64 },
                               oper=A.Plus }

            s1val  = A.OpExp  { left=val'',
                                right=A.IntExp y,
                                oper=A.RShift }
            s2val  = case offset of
                            0 -> A.VarExp var'
                            _ ->  A.OpExp { left=A.VarExp hvar'',
                                            oper=A.Times,
                                            right=A.IntExp offset }
            s3val  = A.OpExp { left=A.VarExp var',
                               right=A.IntExp y,
                               oper=A.LShift }

            s1     = A.sAssign { var=var', val=s1val }
            s2     = A.Assign { var=hvar', val=s2val, op=Just A.Plus }
            s3     = A.Assign { var=hvar'', val=s3val, op=Just A.Minus }

        in [s1, s2, s3] ++ [A.Newline] ++ genFromBytesCarries' xs ys 0


    genFromBytes :: P.Params -> A.Dec
    genFromBytes p =
        let h             = A.Param { pvar="h", ptyp="fe" }
            s             = A.Param { pvar="s", ptyp="const unsigned char *"}

            carry         = A.Var {v="carry", idx=Nothing, typ=Just type64}

            rep'          = rep p
            l             = len p

            loadp         = [4, 3, 3, 3, 3, 4, 3, 3, 3, 3 ] -- specific to curve25519
            cumulative    = scanl1 (+) (0:(rep p))
            reps          = drop (l - 1) (rep' ++ rep')
            indices       = drop (l - 1) ([0.. l - 1] ++ [0..l - 1])
            (oddr, evenr) = oddEvens reps
            (oddi, eveni) = oddEvens indices

            s1            = genFromBytesLoad' loadp cumulative 0 0 (pvar h)
            s2            = genVarDecs' l carry
            s3            = genFromBytesCarries' oddi oddr (offset p)
            s4            = genFromBytesCarries' eveni evenr 0
            s5            = genSimpleAssign l (ParamX h) (ParamX h)

            body'         = A.Seq ( s1 ++ [A.Newline] ++ s2 ++ [A.Newline] ++
                                    s3 ++ [A.Newline] ++ s4 ++ [A.Newline] ++
                                    s5 )

        in A.FuncDec { name   = "fe_frombytes",
                       params = [h, s],
                       rtype  = Nothing,
                       body   = body'}

    ----------------------------------------------------------------------------
    ---------------------------------TO_BYTES-----------------------------------
    ----------------------------------------------------------------------------

    -- load h's value into q
    genToBytesPlace' :: A.Var -> A.Param -> Int -> Int -> A.Exp
    genToBytesPlace' v1 v2 n x =
        let var'  = A.sVar { v=v v1 }
            var'' = A.sVar { v=pvar v2 ++ show n }
            l     = Parens A.OpExp { left=A.VarExp var'',
                                     right=A.VarExp var',
                                     oper=A.Plus }
            val'  = A.OpExp { left  =l,
                              right =A.IntExp x,
                              oper  =A.RShift }

        in  A.sAssign { var = var',
                        val = val' }

    -- each carry is a set of three statements
    genToBytesCarry' :: Int -> Int -> [A.Exp]
    genToBytesCarry' n x =
        let var'  = A.sVar { v="carry" ++ show n }
            h'    = A.sVar { v="h" ++ show n }
            h''   = A.sVar { v="h" ++ show (n + 1) }

            s1val = A.OpExp { left=A.VarExp h',
                              right=A.IntExp x,
                              oper=A.RShift }

            s3val = A.OpExp { left=A.VarExp var',
                              right=A.IntExp x,
                              oper=A.LShift }

            s1    = A.sAssign { var=var', val= s1val }
            s2    = case n of
                        9 -> A.Newline
                        _ -> A.Assign { var =h'',
                                        val = A.VarExp var',
                                        op =Just A.Plus }
            s3    = A.Assign { var=h',
                               val=s3val,
                               op=Just A.Minus }

        in [s1, s2, s3] ++ [A.Newline]

    -- require bytes from h using load pattern and word representation
    -- load into source variable s
    genToBytesMod' :: [Int] -> [Int] -> [Int] -> Int -> Int -> [A.Exp]
    genToBytesMod' [] [] _ _ _ = []
    genToBytesMod' (x:xs) (y:ys) (z:zs) n1 n2 =
        let var'   = A.VarExp A.sVar { v="h" ++ show n1 }
            x1     = A.sVar { v="s", idx=Just (show n2) }
            x2     = A.sVar { v="s", idx=Just (show (n2 + 1)) }
            x3     = A.sVar { v="s", idx=Just (show (n2 + 2)) }
            x4     = case x of
                        4 -> A.sVar { v="s", idx=Just (show (n2 + 3)) }
                        _ -> A.sVar { v="" }

            val'   = 8 * n2 - z  -- how many bits are left over
            a1     = A.OpExp { left=var',
                               right=A.IntExp val',
                               oper=A.RShift}

            a2     = A.OpExp { left=var',
                               right=A.IntExp (val' + 8),
                               oper=A.RShift}

            var''  = A.sVar { v="h" ++ show (n1 + 1) }
            a3base = A.OpExp { left=var',
                               right=A.IntExp (val' + 16),
                               oper=A.RShift }

            left   = case x of
                        4 -> y - (val' + 24)
                        _ -> y - (val' + 16)
            a'     = A.OpExp { left=A.VarExp var'',
                               right=A.IntExp left,
                               oper=A.LShift }

            final  = case left of
                        8 -> a3base
                        _ -> A.OpExp { left=Parens a3base,
                                       right=Parens a',
                                       oper=A.Or }

            a3     = case x of
                        3 -> final
                        _ -> a3base

            a4'    = A.OpExp { left=var',
                               right=A.IntExp (val' + 24),
                               oper=A.RShift }
            a4     = case x of
                        4 -> A.OpExp { left=Parens a4',
                                       right=Parens a',
                                       oper=A.Or }
                        _ -> A.Newline

            s1     = A.sAssign { var=x1, val=a1 }
            s2     = A.sAssign { var=x2, val=a2 }
            s3     = A.sAssign { var=x3, val=a3 }
            s4     = case x of
                        4-> A.sAssign { var=x4, val=a4 }
                        _-> A.Newline

        in [s1, s2, s3, s4] ++ [A.Newline] ++
            genToBytesMod' xs ys zs (n1 + 1) (n2 + x)


    genToBytes :: P.Params -> A.Dec
    genToBytes p =
        let h        = A.Param { pvar="h", ptyp="fe" }
            s        = A.Param { pvar="s", ptyp="unsigned char *"}

            carry    = A.Var { v="carry", idx=Nothing, typ=Just type32 }
            q        = A.Var { v="q", idx=Nothing, typ=Just type32 }
            h'       = A.Var { v="h", idx=Nothing, typ=Just type32 }
            lastvar' = A.sVar { v="h" ++ show (numWords - 1) }

            numWords = len p
            rep'     = rep p
            o        = offset p
            lpattern = [4, 3, 3, 3, 3, 4, 3, 3, 3, 3]
            cumu     = scanl1 (+) (0:(rep p))
            l        = [0..numWords - 1]

            cast'    = A.Typecast { tvar=A.IntExp 1, newtyp=type32}

            mod1     = A.OpExp { left=A.IntExp o,
                                 right=A.VarExp lastvar',
                                 oper=A.Times }
            mod2     = A.OpExp { left=Parens cast',
                                 right=A.IntExp ((last rep' ) - 1),
                                 oper=A.LShift }
            mod      = A.OpExp { left=mod1,
                                 right=Parens mod2,
                                 oper=A.Plus }

            wrap     = A.sAssign { var=A.sVar{ v=v q },
                                   val= A.OpExp { left=mod,
                                   right=A.IntExp (last rep'),
                                   oper=A.RShift } }

            s6'      = A.Assign { var=A.sVar { v="h0" },
                                  val=A.OpExp { left=A.IntExp o,
                                                right=A.VarExp A.sVar{v="q"},
                                                oper=A.Times },
                                  op=Just A.Plus }

            s1       = genSimpleAssign numWords (VarX h') (ParamX h)
            s2       = A.VarDec q
            s3       = genVarDecs' numWords carry
            s4       = wrap
            s5       = zipWith (genToBytesPlace' q h) l rep'
            s6       = s6'
            s7       = concat (zipWith (genToBytesCarry') l rep')
            s8       = genToBytesMod' lpattern rep' cumu 0 0

            body'    = A.Seq ( s1 ++ [A.Newline] ++ [s2] ++ [A.Newline] ++
                               s3 ++ [A.Newline] ++ [s4] ++ [A.Newline] ++
                               s5 ++ [A.Newline] ++ [s6] ++ [A.Newline] ++
                               s7 ++ [A.Newline] )

       in A.FuncDec { name   = "fe_tobytes",
                      params = [s, h],
                      rtype  = Nothing,
                      body   = body'}

    ----------------------------------------------------------------------------
    ---------------------------- MULTIPLICATION --------------------------------
    ----------------------------------------------------------------------------

    genMulPreOffset' :: A.Var -> Int -> Int -> [A.Exp]
    genMulPreOffset' _ _ 0 = []
    genMulPreOffset' v1 offset n =
        let var'  = A.sVar { v= (v v1) ++ show (n - 1) ++ "_" ++ show offset,
                             typ=typ v1}
            var'' = A.sVar { v= (v v1) ++ show (n - 1) }
            val'  = A.OpExp { left=A.IntExp offset,
                              right=A.VarExp var'',
                              oper=A.Times }
            assign = A.sAssign { var=var', val=val' }
        in genMulPreOffset' v1 offset (n - 1) ++ [assign]


    genMulRep' :: Int -> Int -> [Int] -> [Int] -> [Int] -> [[Int]]
    genMulRep' _ _ _ [] _ = []
    genMulRep' n m p (x:xs) (y:ys) =
        -- want to calculate (x + p) - y for all p, y  until p is exhausted
        let l1 = replicate n x
            l  = zipWith (-) (map (`mod` m) (zipWith (+) l1 p)) (y:ys)
        in l : genMulRep' n m p xs ys

    genMulPrecomp' :: A.Param -> [[Int]] -> Int -> [A.Exp]
    genMulPrecomp' _ [] _ = []
    genMulPrecomp' v1 (x:xs) i =
        let var  = A.sVar { v=pvar v1 ++ show i }
            left = delete 0 (nub x)
            l    = genMulPrecomp'' var left
        in l ++ genMulPrecomp' v1 xs (i + 1)

    -- make new precomputations for each fi
    genMulPrecomp'' :: A.Var -> [Int] -> [A.Exp]
    genMulPrecomp'' _ [] = []
    genMulPrecomp'' v1 (x:xs) =
        let var'  = A.sVar { v= v v1 ++ "_" ++ show (2^x), typ=Just type32 }
            op    = A.OpExp { left= A.IntExp (2^x),
                              right= A.VarExp v1,
                              oper= A.Times }

            assign = A.sAssign { var = var', val=op }
        in [assign] ++ genMulPrecomp'' v1 xs


    genMulComps' :: A.Var -> A.Var -> Int -> Int -> Int -> Int -> [Int] -> [A.Exp]
    genMulComps' _ _ _ _ _ _ [] = []
    genMulComps' v1 v2 l o v2idx v1idx (x:xs) =
        let v1'        = A.sVar { v=v v1++show v1idx }
            v2'        = A.sVar { v=v v2++show v2idx }

            (m1, var1) = case x of
                             0 -> (1, "")
                             _ -> (2^x, "_" ++ (show (2^x)))

            (m2, var2) = if (v1idx + v2idx >= l && o > 1)
                            then (o, "_" ++ show o)
                         else (1, "")

            m          = if (m1 == 1 && m2 == 1)
                            then ""
                         else "_" ++ show (m1 * m2)

            finalleft  = A.sVar { v=v v1'++ v v2' ++ m }
            finalv1    = A.sVar { v=v v2' ++ var2 }
            finalv2    = A.sVar { v=v v1' ++ var1 }

            cast       = A.Typecast { tvar= A.VarExp finalv1, newtyp=type64 }
            op         = A.OpExp    { left= A.VarExp finalv2,
                                      right= cast,
                                      oper=A.Times }
            assign     = A.sAssign  { var=finalleft, val=op }

        in [assign] ++ genMulComps' v1 v2 l o (v2idx + 1) v1idx xs

    genMulSums' :: A.Var -> A.Var -> A.Param -> Int -> Int -> [[Int]] -> Int -> [A.Exp]
    genMulSums' _ _ _ _ _ [] _ = []
    genMulSums' v1 v2 out l o full idx =
        let var' = A.sVar { v=pvar out ++ show idx, typ=Just type64 }
        in [ A.sAssign { var=var',
                         val= genMulSums'' v1 v2 0 idx l o full } ]

    genMulSums'' :: A.Var -> A.Var -> Int -> Int -> Int -> Int -> [[Int]] -> A.Exp
    genMulSums''  _ _ _ _ _ _ [] = A.Newline
    genMulSums'' v1 v2 v1idx v2idx l o (x:xs) =
        -- corrects for negative indices by wrapping around
        let _2idx = (v2idx + l) `mod` l
            s     = if (v2idx < 0)
                        then o
                    else 1

            x'    = s * (2 ^ (head (drop _2idx x)))
            subs  = case x' of
                         1 -> ""
                         _ -> "_" ++ show x'
            exp   = v v1 ++ show v1idx ++ v v2 ++ show _2idx ++ subs
            var'  = A.sVar { v=exp }

            rest  = genMulSums'' v1 v2 (v1idx + 1) (v2idx - 1) l o xs
            final = if v1idx + 1 >= l
                        then A.VarExp var'
                    else A.OpExp { left=A.VarExp var', oper=A.Plus, right=rest }
        in final

    -- as far as i know, not generalized for all primes
    genMulCarries' :: A.Var -> A.Var -> Int -> [Int] -> Int -> Int -> [A.Exp]
    genMulCarries' v1 cv l r' o idx =
        let r       = r'!! idx
            v'      = A.sVar { v= v v1 ++ show idx}
            v1'     = A.sVar { v= v v1 ++ show ((idx + 1) `mod` l) }
            var'    = A.sVar { v=v cv ++ show idx }

            s1shift = A.OpExp { left=A.IntExp 1,
                                right=A.IntExp (r - 1),
                                oper=A.LShift}
            tvar'   = A.Typecast { tvar= Parens s1shift, newtyp=type64 }
            s1''    = A.OpExp { left=A.VarExp v', right=tvar', oper=A.Plus }

            s1'     = A.OpExp { left=Parens s1'',
                                right=A.IntExp r,
                                oper=A.RShift }
            s1      = A.sAssign { var=var', val=s1' }

            s2'     = if (idx + 1 >= l)
                        then A.OpExp { left=A.VarExp var',
                                       right=A.IntExp o,
                                       oper=A.Times }
                      else A.VarExp var'
            s2      = A.Assign { var=v1', val=s2', op=Just A.Plus}
            s3'     = A.OpExp { left=A.VarExp var',
                                right=A.IntExp r,
                                oper=A.LShift}
            s3      = A.Assign { var=v', val= s3', op=Just A.Minus }

        in [s1, s2, s3, A.Newline]


    genMul ::  P.Params -> A.Dec
    genMul p =
        let h        = A.Param { pvar="h", ptyp="fe" }
            f        = A.Param { pvar="f", ptyp="fe" }
            g        = A.Param { pvar="g", ptyp="fe" }

            f'       = A.Var { v="f", idx=Nothing, typ=Just type32 }
            g'       = A.Var { v="g", idx=Nothing, typ=Just type32 }
            h'       = A.Var { v="h", idx=Nothing, typ=Just type32 }
            carry    = A.Var { v="carry", idx=Nothing, typ=Just type64 }

            numWords = len p
            o        = offset p
            places   = init ( 0 : (scanl1 (+) (rep p)) )
            l1       = [0 .. numWords  - 1]
            r'       = genMulRep' numWords (base p) places places (places ++ places)

            s1       = genSimpleAssign numWords (VarX f') (ParamX f)
            s2       = genSimpleAssign numWords (VarX g') (ParamX g)
            s3       = genMulPreOffset' g' o numWords
            s4       = genMulPrecomp' f r' 0
            zipped   = zipWith ( genMulComps' f' g' numWords o 0 ) l1 r'
            s5       = concat zipped
            s6       = concat (map ( genMulSums' f' g' h numWords o r') l1)

            -- TODO: PLACEHOLDER ARRAY
            w        = [0, 4, 1, 5, 2, 6, 3, 7, 4, 8, 9, 0]
            s7       = genVarDecs' numWords carry
            s8       = concat (map (genMulCarries' h' carry numWords (rep p) o) w )
            s9       = genSimpleAssign numWords (ParamX h) (VarX h')

            body'    = A.Seq ( s1 ++ [A.Newline] ++ s2 ++ [A.Newline] ++
                               s3 ++ [A.Newline] ++ s4 ++ [A.Newline] ++
                               s5 ++ [A.Newline] ++ s6 ++ [A.Newline] ++
                               s7 ++ [A.Newline] ++ s8 ++ s9 )

        in A.FuncDec { name   = "fe_mul",
                       params = [h, f, g],
                       rtype  = Nothing,
                       body   = body' }

    ----------------------------------------------------------------------------
    -------------------------------- SQUARING ----------------------------------
    ----------------------------------------------------------------------------

    -- rearrange results of genMulRep' so that each list contains the
    -- prospective coefficients of each h_i
    genSquareReindex' :: [[Int]] -> Int -> Int -> [Int]
    genSquareReindex' [] _ _  = []
    genSquareReindex' (x:xs) l idx =
        head (drop (idx `mod` l)  x) : genSquareReindex' xs l (idx - 1)

    genSquareRed' :: [Int] -> [Int] -> [Int]
    genSquareRed' [] [] = []
    genSquareRed' (y:ys) (x:xs) =
        if ( y /= 0 && (x `rem` y == 0))
            then x `div` y : genSquareRed' ys xs
        else x : genSquareRed' ys xs

    genSquarePrecomp' :: [Int] -> Int
    genSquarePrecomp' a =
        case filter ( > 1 ) a of
            [] -> 0
            _ -> minimum (filter ( > 1 ) a)

    genSquareOffsets' ::A.Param ->  (Int, Int) -> A.Exp
    genSquareOffsets' v' (value, idx) =
        let var' = A.sVar { v=pvar v' ++ show idx ++ "_" ++ show value, typ=Just type32 }
            val' = A.OpExp { left=A.VarExp A.sVar { v=show value },
                             right=A.VarExp A.sVar { v=pvar v' ++ show idx } ,
                             oper=A.Times }
        in A.sAssign { var=var', val=val' }

    -- black voodoo magic
    genSquareMul' :: [Int] -> A.Var -> Int -> Int -> [Int] -> [Int] -> Int -> [A.Exp]
    genSquareMul' _ _ _ _ _ [] _ = []
    genSquareMul' (y:ys) v1 i l (o:os) (x:xs) idx =
        let i' = (l + idx - i) `mod` l
            lvar' = if (i + i' >= l)
                        then case (x > 1 && (x `div` o) > 1) of
                                True -> "_" ++ show (x `div` o)
                                _ -> ""
                    else case y > 1 && x /= 1 of
                        True -> "_" ++ show y
                        _ -> ""

            rvar' = if (i + i' >= l && o > 1)
                        then "_" ++ show o
                    else if (x < y && x /= 1 && x > 1)
                        then "_" ++ show x
                    else case (y /= 0) && (x `div` y) > 1 && x > 1 of
                            True -> "_" ++ show (x `div` y)
                            _ -> ""

            left' = A.VarExp A.sVar {v=v v1 ++ show i' ++ lvar'}
            right' = A.Typecast { tvar=A.VarExp A.sVar { v=v v1++show i++rvar' }, newtyp=type64}
            var'' = if (i + i' >= l && x > 1)
                        then "_" ++ show x
                    else if (x /= 1)
                        then "_" ++ show x
                    else ""
            var' = A.sVar{ v=v v1++show i'++v v1++show i++var'', typ=Just type64 }
            val' = A.OpExp { left=left', right=right', oper=A.Times}
            final = genSquareMul' ys v1 (i + 1) l os xs idx
        in if (i' > i || x == 0)
                then final
            else A.sAssign { var=var', val=val' } : final

    genSquareSums' :: A.Var -> A.Var -> Int -> [Int] -> Int -> A.Exp
    genSquareSums' v1 v2 l ca idx =
        A.sAssign { var=A.sVar { v= v v2 ++ show idx, typ=Just type64},
                    val=A.Seq (genSquareSums'' ca v1 0 l idx) }

    genSquareSums'' :: [Int] -> A.Var -> Int -> Int -> Int-> [A.Exp]
    genSquareSums'' [] _ _ _ _ = []
    genSquareSums'' (x:xs) v1 i l idx =
        let i' = (l + idx - i) `mod` l
            s' = if (i + i' >= l || x /= 1)
                    then "_" ++ show x
                 else ""
            sum = A.VarExp A.sVar {v=v v1++show i'++v v1++show i++ s'}
            final = genSquareSums'' xs v1 (i + 1) l idx
        in if null final
                then [sum] ++ [A.VarExp A.sVar{v="done"}]
            else if (i' > i || x == 0)
                then final
            else [A.OpExp { left=sum, oper=A.Plus, right=head final }]

    genSquareCoeffs :: A.Var -> [Int] -> Int -> [A.Exp]
    genSquareCoeffs _ [] _ = []
    genSquareCoeffs v1 (x:xs) idx =
        let var' = A.sVar { v=v v1 ++ show idx ++ "_" ++ show x, typ=Just type32}
            val' = A.OpExp { left=A.IntExp x,
                             right=A.VarExp A.sVar {v = v v1 ++ show idx},
                             oper=A.Times }
            final = genSquareCoeffs v1 xs idx
        in if (x == 1)
                then final
           else A.sAssign { var=var', val=val' } : final

    genSquareCoeffs'' :: Int -> Int -> [Int] -> [Int] -> [[Int]] -> Int -> [Int] -> [[Int]]
    genSquareCoeffs'' _ _ _ _ p _ [] = p
    genSquareCoeffs'' i l (o:os) (y:ys) precomp idx (x:xs) =
        let final = genSquareCoeffs'' (i + 1) l os ys precomp idx xs
            i' = (l + idx - i) `mod` l
            ps = if (i + i' >= l)
                    then anth final i' (x `div` o)
                 else if x == 1
                    then final
                 else if (x < y)
                    then anth final i x
                 else anth (anth final i (x `div` y)) i' y
        in if (i' > i || x == 0)
                then final
            else ps ++ final

    anth :: [[Int]] -> Int -> Int -> [[Int]]
    anth [] _ _ = []
    anth (l:ls) idx entry =
        if (idx == 0)
            then ( (nub (entry : l)) :ls)
        else l : (anth ls (idx-1) entry)
        -- I FAILED I'M SORRY
        --    else h ++ (nub (entry : if (t== []) then [] else head t) : tail t)

    genSquareCoeffsWrapper :: [[Int]] -> [Int] -> [[Int]] -> Int -> [Int] -> Int -> [[Int]]
    genSquareCoeffsWrapper [] _ p _ _ _ = p
    genSquareCoeffsWrapper (x:xs) pca precomp l moa idx =
        genSquareCoeffsWrapper xs pca (genSquareCoeffs'' 0 l moa pca precomp idx x ) l moa (idx + 1)

    genSquareMins' :: Int -> Int -> [Int] -> Int
    genSquareMins' l idx list =
        let t = filter (>0) (take (idx `mod` l) list)
        in if (t == [])
              then 0
            else minimum t

    genSquareHCoeffs' :: Int -> Int -> Int -> Int -> [Int] -> [Int]
    genSquareHCoeffs'  _ _ _ _ [] = []
    genSquareHCoeffs' l o i idx (x:xs) =
        let i'    = (l + idx - i) `mod` l
            m     = if (i' /= i)
                       then 2
                    else 1
            m'    = if (i' + i >= l)
                      then o
                    else 1
            final = genSquareHCoeffs' l o (i + 1) idx xs
        in if i' > i
                then 0 : final
            else (m * m' * (2^x)) : final


    genSquare ::  P.Params -> A.Dec
    genSquare p =
        let h        = A.Param { pvar="h", ptyp="fe" }
            f        = A.Param { pvar="f", ptyp="fe" }

            h'       = A.Var { v="h", idx=Nothing, typ=Just type32 }
            f'       = A.Var { v="f", idx=Nothing, typ=Just type32 }
            carry    = A.Var { v="carry", idx=Nothing, typ=Just type32 }

            numWords = len p
            o        = offset p
            l        = [0..numWords - 1]
            empty    = [ [] |  x <- [ 1.. numWords] ]
            pl       = init ( 0 : (scanl1 (+) (rep p)) )

            ra       = genMulRep' numWords (base p) pl pl (pl ++ pl)
            ra'      = map (genSquareReindex' ra numWords) l
            ca       = zipWith (genSquareHCoeffs' numWords o 0) l ra'
            moa      = zipWith (genSquareMins' numWords) l (transpose ca)
            rca      = map (genSquareRed' moa) ca
            pca      = map (genSquarePrecomp') (transpose rca)
            moo'     = [ (x, y) | (x, y) <- zip moa l, x /= 0 ]

            s1       = genSimpleAssign numWords (VarX f') (ParamX f)
            inter    = genSquareCoeffsWrapper ca pca [[]] numWords moa 0
            s2       = concat (zipWith (genSquareCoeffs f') inter l)
            s3       = map (genSquareOffsets' f) moo'
            s4       = concat (zipWith (genSquareMul' pca f' 0 numWords moa) ca l)
            s5       = zipWith (genSquareSums' f' h' numWords) ca l
            s6       = genVarDecs' numWords carry
            -- TODO
            w        = [0, 4, 1, 5, 2, 6, 3, 7, 4, 8, 9, 0]
            s7       = concat (map (genMulCarries' h' carry numWords (rep p) o) w )
            s8       = genSimpleAssign numWords (ParamX h) (VarX h')

            body'    = A.Seq  ( s1 ++ [A.Newline] ++ s2 ++ [A.Newline] ++
                                s3 ++ [A.Newline] ++ s4 ++ [A.Newline] ++
                                s5 ++ [A.Newline] ++ s6 ++ [A.Newline] ++
                                s7 ++ s8 )

        in A.FuncDec { name   = "fe_sq",
                       params = [h, f],
                       rtype  = Nothing,
                       body   = body' }

    ----------------------------------------------------------------------------
    ------------------------------ INVERTING -----------------------------------
    ----------------------------------------------------------------------------
    -- source code:
    -- github.com/jedisct1/libsodium/blob/master/src/libsodium/crypto_scalarmult/curve25519/ref10/fe_invert_curve25519_ref10.c
    -- TODO: there seems to be some things missing...

    genInvert :: P.Params -> A.Dec
    genInvert p =
        let out   = A.Param { pvar="out", ptyp="fe"}
            z     = A.Param { pvar="z", ptyp="fe" }

            body' = A.Return ( A.VarExp A.sVar{v="0"} )

        in A.FuncDec { name   = "fe_invert",
                       params = [out, z],
                       rtype  = Nothing,
                       body   = body' }

    ----------------------------------------------------------------------------
    ----------------------------- HELPER FUNCTIONS -----------------------------
    ----------------------------------------------------------------------------

    -- assign v1 to v2
    -- usually assigning to local variable from parameter,
    -- or vice versa
    genSimpleAssign :: Int -> A.GenVar -> A.GenVar -> [A.Exp]
    genSimpleAssign 0 _ _ = []
    genSimpleAssign n v1 v2 =
        let var'  = case v1 of
                        ParamX (A.Param { pvar=pv, ptyp=_ } ) ->
                            A.sVar { v=pv, idx = Just (show (n - 1)) }
                        VarX (A.Var { v=v1n, idx=_, typ=t } ) ->
                            A.sVar { v=v1n ++ show (n - 1), typ=t}

            val'  = case v2 of
                        ParamX (A.Param { pvar=pv, ptyp=_ } ) ->
                            A.sVar { v=pv, idx=Just (show (n - 1)) }
                        VarX (A.Var { v=v2n, idx=_, typ=_ } ) ->
                            A.sVar { v=v2n ++ show (n - 1) }

            assign = A.sAssign { var=var', val=A.VarExp (val') }
        in genSimpleAssign (n - 1) v1 v2 ++ [assign]


    -- similar to simpleAssign, but with assignments of the form
    -- v1 = v2 oper' v3
    genOper :: Int -> A.GenVar -> A.GenVar -> A.GenVar -> A.Op -> [A.Exp]
    genOper 0 _ _ _ _  = []
    genOper n v1 v2 v3 oper' =
        let id     = case v1 of
                        ParamX (A.Param { pvar=_, ptyp=_ })
                          -> Just (show (n - 1))
                        _ -> Nothing
            vtyp   = case v1 of
                        VarX (A.Var { v=_, idx=_, typ=t }) -> t
                        _ -> Nothing
            vname  = case v1 of
                        ParamX (A.Param { pvar=v1name, ptyp=_ }) -> v1name
                        VarX (A.Var { v=v1n, idx=_, typ=_ })
                          -> v1n ++ show (n - 1)
            v2'    = case v2 of
                        VarX (A.Var { v=v2name, typ=_ }) -> v2name
                        ParamX (A.Param { pvar=v2n, ptyp=_ }) -> v2n
            v3'    = case v3 of
                        VarX (A.Var { v=v3name, typ=_ }) -> v3name
                        ParamX (A.Param { pvar=v3n, ptyp=_ }) -> v3n

            var'   = A.Var { v=vname, idx=id, typ=vtyp}
            val'   = A.OpExp { left=A.VarExp A.sVar{ v=v2' ++ show (n - 1) },
                               oper=oper',
                               right=A.VarExp A.sVar{ v=v3' ++ show (n - 1) } }

            assign = A.sAssign { var=var', val=val' }

        in genOper (n - 1) v1 v2 v3 oper' ++ [assign]

    -- generates variable declarations for variables of name v1++[1..n-1]
    genVarDecs' :: Int -> Var -> [A.Exp]
    genVarDecs' 0 _  = []
    genVarDecs' n v1 =
        let var' = A.sVar { v = (v v1) ++ (show (n - 1)), typ=(typ v1)}
        in genVarDecs' (n - 1) v1 ++ [A.VarDec var']


    -- generates a sequence of assignments of the following form:
    -- v1 op= v2
    genAll' :: Int -> A.Var -> A.Param -> A.Op -> [A.Exp]
    genAll' 0 _ _ _ = []
    genAll' n v1 v2 op' =
        let var'   = A.sVar {v=(v v1) ++ (show (n - 1)) }
            val'   = A.VarExp A.sVar { v=pvar v2 }
            assign = A.Assign { var=var', val=val', op=Just op' }
        in genAll' (n - 1) v1 v2 op' ++ [assign]

    --- small helper function for extracting odd, even elements of lists
    oddEvens :: [Int] -> ([Int], [Int])
    oddEvens [] = ([], [])
    oddEvens [x] = ([x], [])
    oddEvens (x:y:xs) = (x:xp, y:yp) where (xp, yp) = oddEvens xs
