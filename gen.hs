module Gen where

    import Data.List

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
            assign = A.Assign { var=var', val=A.IntExp 0, op =Nothing }
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
        let val' = case n of
                    1 -> A.IntExp 1
                    _ -> A.IntExp 0
            var' = A.Var { v=pvar v', idx=Just (show (n - 1)), typ=Nothing}
            assign = A.Assign { var=var', val=val', op =Nothing }
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
        let h = A.Param { pvar="h", ptyp="fe" }
            f = A.Param { pvar="f", ptyp="fe" }
            g = A.Param { pvar="g", ptyp="fe" }

            f' = A.Var { v="f", idx=Nothing, typ=Just type32 }
            g' = A.Var { v="g", idx=Nothing, typ=Just type32 }
            h' = A.Var { v="h", idx=Nothing, typ=Just type32 }

            s1 = genSimpleAssign numWords (VarX f') (ParamX f)
            s2 = genSimpleAssign numWords (VarX g') (ParamX g)
            s3 = genOper numWords (VarX h') (ParamX f) (ParamX g) A.Plus
            s4 = genSimpleAssign numWords (ParamX h) (VarX h')

            body' = A.Seq ( s1 ++ s2 ++ s3 ++ s4 )

        in A.FuncDec { name   = "fe_add",
                       params = [h, f, g],
                       rtype  = Nothing,
                       body   = body' }

    --------------------------------------------------------------------------

    genSub :: Int -> A.Dec
    genSub numWords =
        let h = A.Param { pvar="h", ptyp="fe" }
            f = A.Param { pvar="f", ptyp="fe" }
            g = A.Param { pvar="g", ptyp="fe" }

            f' = A.Var { v="f", idx=Nothing, typ=Just type32 }
            g' = A.Var { v="g", idx=Nothing, typ=Just type32 }
            h' = A.Var { v="h", idx=Nothing, typ=Just type32 }

            s1 = genSimpleAssign numWords (VarX f') (ParamX f)
            s2 = genSimpleAssign numWords (VarX g') (ParamX g)
            s3 = genOper numWords (VarX h') (ParamX f) (ParamX g) A.Minus
            s4 = genSimpleAssign numWords (ParamX h) (VarX h')

            body' = A.Seq ( s1 ++ s2 ++ s3 ++ s4 )

        in A.FuncDec { name   = "fe_sub",
                       params = [h, f, g],
                       rtype  = Nothing,
                       body   = body' }

    ----------------------------------------------------------------------------

    genCopy :: Int -> A.Dec
    genCopy numWords =
        let f = A.Param { pvar="f", ptyp="fe" }
            h = A.Param { pvar="h", ptyp="fe" }

            f' = A.Var { v="f", idx=Nothing, typ=Just type32 }

            s1 = genSimpleAssign numWords (VarX f') (ParamX f)
            s2 = genSimpleAssign numWords (ParamX h) (VarX f')

            body' = A.Seq ( s1 ++ s2 )

        in A.FuncDec { name   = "fe_copy",
                       params = [h, f],
                       rtype  = Nothing,
                       body   = body'}

    ----------------------------------------------------------------------------

    genSwap :: Int -> A.Dec
    genSwap numWords =
        let f = A.Param { pvar="f", ptyp="fe" }
            g = A.Param { pvar="g", ptyp="fe" }
            b = A.Param { pvar="b", ptyp="unsigned int" }

            f' = A.Var { v="f", idx=Nothing, typ=Just type32 }
            g' = A.Var { v="g", idx=Nothing, typ=Just type32 }
            x  = A.Var { v="x", idx=Nothing, typ=Just type32 }
            b' = A.Var { v= pvar b, idx=Nothing, typ=Nothing }

            s1 = genSimpleAssign numWords (VarX f') (ParamX f)
            s2 = genSimpleAssign numWords (VarX g') (ParamX g)
            s3 = genOper numWords (VarX x) (VarX f') (VarX g') A.ExOr
            s4 = A.Assign {var=b', val=Negate b', op=Nothing }
            s5 = genAll' numWords x b A.And
            s6 = genOper numWords (ParamX f) (VarX f') (VarX x) A.ExOr
            s7 = genOper numWords (ParamX g) (VarX g') (VarX x) A.ExOr

            body' = A.Seq ( s1 ++ s2 ++ s3 ++ [s4] ++ s5 ++ s6 ++ s7 )

        in A.FuncDec { name   = "fe_cswap",
                       params = [f, g, b],
                       rtype  = Nothing,
                       body   = body' }

    ----------------------------------------------------------------------------

    genLoad' :: Int -> Int -> A.Var -> A.Param -> [A.Exp]
    genLoad' 0 0 v1 v2 =
        let var' = A.Var{v=pvar v2, idx=Just (show 0), typ=Nothing}
            val' = A.Typecast{tvar=A.VarExp var' ,
                              newtyp=utype64}
        in [A.Assign { var=v1, val=val', op=Nothing } ]

    genLoad' n x v1 v2 =
        let var' = A.Var{v=pvar v2, idx=Just (show x), typ=Nothing }
            val' = Parens ( A.Typecast { tvar=A.VarExp var' ,
                                         newtyp=utype64})
            assign = A.Assign { var=v1,
                                val= A.OpExp{ left=val', right= A.IntExp n, oper=A.LShift },
                                op=Just A.Or }
        in genLoad' (n - 8) (x - 1) v1 v2 ++ [assign]

    genLoad3 :: A.Dec
    genLoad3 =
        let param  = A.Param { pvar="in", ptyp="const unsigned char *" }
            result = A.Var { v="result", idx=Nothing, typ=Just utype64 }

            s1 = [ A.VarDec result ]
            s2 = genLoad' 16 2 result param
            s3 = [ A.Return (A.VarDec result) ]

            body' = A.Seq ( s1 ++ s2 ++ s3 )

        in A.FuncDec { name   ="load_3",
                       params = [param],
                       rtype  = Just "static crypto_uint64",
                       body   = body' }

    genLoad4 :: A.Dec
    genLoad4 =
        let param  = A.Param { pvar="in", ptyp="const unsigned char *" }
            result = A.Var { v="result", idx=Nothing, typ=Just utype64 }

            s1 = [ A.VarDec result ]
            s2 = genLoad' 24 3 result param
            s3 = [ A.Return (A.VarDec result) ]

            body' = A.Seq ( s1 ++ s2 ++ s3 )

        in A.FuncDec { name   = "load_4",
                       params = [param],
                       rtype  = Just "static crypto_uint64",
                       body   = body'}

    ----------------------------------------------------------------------------
    -------------------------------FROM_BYTES-----------------------------------
    ----------------------------------------------------------------------------

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
            assign = A.Assign { var=var', val=val', op=Nothing }
        in [assign] ++ genFromBytesLoad' xs ys (n + 1) (cumu + x) name


    genFromBytesCarries' :: [Int] -> [Int] -> Int -> [A.Exp]
    genFromBytesCarries' [] [] _ = []
    genFromBytesCarries' (x:xs) (y:ys) offset =
        let var'   = A.Var{ v="carry" ++ show x, idx=Nothing, typ=Nothing}
            hvar'  = case offset of
                0 -> A.Var{ v="h" ++ (show (x+1)), idx=Nothing, typ=Nothing}
                _ -> A.Var{ v="h0", idx=Nothing, typ=Nothing}
            hvar'' = A.Var{v="h" ++ show x, idx=Nothing, typ=Nothing}

            shiftExp = A.OpExp { left=A.IntExp 1,
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

            s1 = A.Assign { var=var', val=s1val, op=Nothing }
            s2 = A.Assign { var=hvar', val=s2val, op=Just A.Plus }
            s3 = A.Assign { var=hvar'', val=s3val, op=Just A.Minus }

        in [s1, s2, s3] ++ [A.Newline] ++ genFromBytesCarries' xs ys 0
    genFromBytesCarries' _ _ _ = error ("oh noes, incorrect format")

    genFromBytes :: P.Params -> A.Dec
    genFromBytes p =
        let rep' = rep p
            l = len p

            h           = A.Param { pvar="h", ptyp="fe" }
            s           = A.Param { pvar="s", ptyp="const unsigned char *"}
            loadp       = [4, 3, 3, 3, 3, 4, 3, 3, 3, 3 ]
            cumulative  = scanl1 (+) (0:(rep p))

            reps        = drop (l - 1) (rep' ++ rep')
            indices     = drop (l - 1) ([0.. l - 1] ++ [0..l - 1])
            (oddreps, evenreps) =  oddEvens reps
            (oddi, eveni) = oddEvens indices

            carry       = A.Var {v="carry", idx=Nothing, typ=Just type64}

            s1 = genFromBytesLoad' loadp cumulative 0 0 (pvar h)
            s2 = genVarDecs' l carry
            s3 = genFromBytesCarries' oddi oddreps (offset p)
            s4 = genFromBytesCarries' eveni evenreps 0
            s5 = genSimpleAssign l (ParamX h) (ParamX h)

            body' = A.Seq ( s1 ++ [A.Newline] ++
                            s2 ++ [A.Newline] ++
                            s3 ++ [A.Newline] ++
                            s4 ++ [A.Newline] ++
                            s5 )

        in A.FuncDec { name   = "fe_frombytes",
                       params = [h, s],
                       rtype  = Nothing,
                       body   = body'}

    ----------------------------------------------------------------------------
    ---------------------------------TO_BYTES-----------------------------------
    ----------------------------------------------------------------------------

    genToBytesPlace' :: Int -> [Int] -> A.Var -> A.Param -> [A.Exp]
    genToBytesPlace' _ [] _ _ = []
    genToBytesPlace' n (x:xs) v1 v2 =
        let var'  = A.Var { v=v v1 , idx=Nothing, typ=Nothing }
            var'' = A.Var { v=pvar v2 ++ show n, idx=Nothing, typ=Nothing}
            l     = Parens A.OpExp { left=A.VarExp var'',
                                 right=A.VarExp var',
                                 oper=A.Plus }
            r     = A.OpExp { left  =l,
                              right =A.IntExp x,
                              oper  =A.RShift }
            assign = A.Assign { var = A.Var{v=v v1, idx=Nothing, typ=Nothing },
                                val = r,
                                op=Nothing }
        in  [assign] ++ genToBytesPlace' (n + 1) xs v1 v2

    genToBytesCarry' :: Int -> [Int] -> [A.Exp]
    genToBytesCarry' _ [] = []
    genToBytesCarry' n (x:xs) =
        let var' = A.Var { v="carry" ++ show n, typ=Nothing, idx=Nothing }
            h'   = A.Var { v="h" ++ show n, typ=Nothing, idx=Nothing }
            h''  = A.Var { v="h" ++ show (n + 1), typ=Nothing, idx=Nothing }

            s1val = A.OpExp { left=A.VarExp h', right=A.IntExp x, oper=A.RShift }
            s3val = A.OpExp { left=A.VarExp var', right=A.IntExp x, oper=A.LShift }

            s1    = A.Assign { var=var', val= s1val, op=Nothing }
            s2    = case n of
                    9 -> A.Newline
                    _ -> A.Assign { var =h'',
                                    val = A.VarExp var',
                                    op =Just A.Plus }
            s3 = A.Assign { var=h',
                            val=s3val,
                            op=Just A.Minus }

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
            a1  = A.OpExp { left=var',
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
                _ -> A.OpExp { left=Parens a3base, right=Parens a', oper=A.Or }

            a3 = case x of
                    3 -> final
                    _ -> a3base
            a4' = A.OpExp { left=var',
                            right=A.IntExp (val' + 24),
                            oper=A.RShift }
            a4 = case x of
                    4 -> A.OpExp { left=Parens a4', right=Parens a', oper=A.Or }
                    _ -> A.Newline

            s1 = A.Assign { var=x1, val=a1, op=Nothing }
            s2 = A.Assign { var=x2, val=a2, op=Nothing }
            s3 = A.Assign { var=x3, val=a3, op=Nothing }
            s4 = case x of
                    4-> A.Assign { var=x4, val=a4, op=Nothing }
                    _-> A.Newline

        in [s1, s2, s3, s4] ++ [A.Newline] ++ genToBytesMod' xs ys zs (n1 + 1) (n2 + x)


    genToBytes :: P.Params -> A.Dec
    genToBytes p =
        let h = A.Param { pvar="h", ptyp="fe" }
            s = A.Param { pvar="s", ptyp="unsigned char *"}

            numWords = len p
            rep' = rep p
            o = offset p

            loadpattern = [4, 3, 3, 3, 3, 4, 3, 3, 3, 3]
            cumulative = scanl1 (+) (0:(rep p))

            carry = A.Var {v="carry", idx=Nothing, typ=Just type32}
            q = A.Var{v="q", idx=Nothing, typ=Just type32}
            h' = A.Var {v="h", idx=Nothing, typ=Just type32}
            lastvar' = A.Var { v="h"++show (numWords - 1), idx=Nothing, typ=Nothing }

            mod1  = A.OpExp { left=A.IntExp o,
                             right=A.VarExp lastvar',
                             oper=A.Times }

            cast' = A.Typecast { tvar=A.IntExp 1,
                                 newtyp=type32}
            mod2  = A.OpExp { left= Parens cast',
                              right=A.IntExp ((last rep' ) - 1),
                              oper=A.LShift }

            mod  = A.OpExp { left=mod1,
                             right= Parens mod2,
                             oper=A.Plus }

            wrap = A.Assign { var= A.Var{ v=v q, idx=Nothing, typ=Nothing },
                              val= A.OpExp { left=mod,
                                              right=A.IntExp (last rep'),
                                              oper=A.RShift },
                              op=Nothing }

            s1 = genSimpleAssign numWords (VarX h') (ParamX h)
            s2 = [ A.VarDec q ]
            s3 = genVarDecs' numWords carry
            s4 = [ wrap ]
            s5 = genToBytesPlace' 0 rep' q h
            s6 = genToBytesCarry' 0 rep'
            s7 = genToBytesMod' loadpattern rep' cumulative 0 0

            body' = A.Seq ( s1 ++ s2 ++ s3 ++ [A.Newline] ++
                            s4 ++ [A.Newline] ++
                            s5 ++ [A.Newline] ++
                            s6 ++ [A.Newline] ++
                            s7 )

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
        let var' = A.Var { v= (v v1) ++ show (n - 1) ++ "_" ++ show offset,
                           idx=Nothing,
                           typ=typ v1}
            var'' = A.Var { v= (v v1) ++ show (n - 1), idx=Nothing, typ=Nothing}
            val' = A.OpExp { left=A.IntExp offset,
                             right=A.VarExp var'',
                             oper=A.Times }
            assign = A.Assign { var=var', val=val', op=Nothing }
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
        let var = A.Var { v=pvar v1 ++ show i, idx=Nothing, typ=Nothing }
            leftovers = delete 0 (nub x)
            l = genMulPrecomp'' var leftovers
        in l ++ genMulPrecomp' v1 xs (i + 1)

    -- make new precomputations for each fi
    genMulPrecomp'' :: A.Var -> [Int] -> [A.Exp]
    genMulPrecomp'' _ [] = []
    genMulPrecomp'' v1 (x:xs) =
        let var' = A.Var { v= v v1 ++ "_" ++ show (2^x) , idx=Nothing, typ=Just type32 }
            op = A.OpExp { left= A.IntExp (2^x),
                           right= A.VarExp v1,
                           oper= A.Times }
            assign = A.Assign { var = var', val=op, op=Nothing }
        in [assign] ++ genMulPrecomp'' v1 xs


    genMulComps' :: A.Var -> A.Var -> Int -> Int -> Int -> Int -> [Int] -> [A.Exp]
    genMulComps' _ _ _ _ _ _ [] = []
    genMulComps' v1 v2 l o v2idx v1idx (x:xs) =
        let v1' = A.Var {v=v v1++show v1idx, idx=Nothing, typ=Nothing}
            v2' = A.Var {v=v v2++show v2idx, idx=Nothing, typ=Nothing}

            (m1, var1) = if (x /= 0)
                            then (2^x, "_" ++ (show (2^x)))
                         else (1, "")
            (m2, var2) = if (v1idx + v2idx >= l && o > 1)
                            then (o, "_" ++ show o)
                         else (1, "")
            m = if (m1 == 1 && m2 == 1)
                    then ""
                else "_" ++ show (m1 * m2)

            finalleft = A.Var { v=v v1'++ v v2' ++ m, idx=Nothing, typ=Nothing }
            finalv1   = A.Var { v=v v2' ++ var2, idx=Nothing, typ=Nothing }
            finalv2   = A.Var { v=v v1' ++ var1, idx=Nothing, typ=Nothing }

            cast      = A.Typecast { tvar= A.VarExp finalv1, newtyp=type64 }
            op        = A.OpExp    { left= A.VarExp finalv2, right= cast, oper=A.Times }
            assign    = A.Assign   { var=finalleft, val=op, op=Nothing }

        in [assign] ++ genMulComps' v1 v2 l o (v2idx + 1) v1idx xs

    genMulSums' :: [A.Exp]
    genMulSums' = [A.Newline]

    genMulCarries' :: [A.Exp]
    genMulCarries' = [A.Newline]

    genMul ::  P.Params -> A.Dec
    genMul p =
        let h = A.Param { pvar="h", ptyp="fe" }
            f = A.Param { pvar="f", ptyp="fe" }
            g = A.Param { pvar="g", ptyp="fe" }

            f' = A.Var { v="f", idx=Nothing, typ=Just type32 }
            g' = A.Var { v="g", idx=Nothing, typ=Just type32 }
            h' = A.Var { v="h", idx=Nothing, typ=Just type32 }
            carry = A.Var { v="carry", idx=Nothing, typ=Just type64 }

            numWords = len p
            o = offset p
            r = rep p
            mod = base p

            accu = scanl1 (+) r
            places = init ( 0 : accu )
            r' = genMulRep' numWords mod places places (places ++ places)
            l1 = [0 .. numWords  - 1]

            s1 = genSimpleAssign numWords (VarX f') (ParamX f)
            s2 = genSimpleAssign numWords (VarX g') (ParamX g)
            s3 = genMulPreOffset' g' o numWords
            s4 = genMulPrecomp' f r' 0

            zipped = zipWith ( genMulComps' f' g' numWords o 0 ) l1 r'
            s5 = concat zipped
            s6 = genMulSums'
            s7 = genMulCarries'
            s9 = genVarDecs' numWords carry
            s8 = genSimpleAssign numWords (ParamX h) (VarX h')

            body' = A.Seq ( s1 ++ [A.Newline] ++ s2 ++ [A.Newline] ++
                            s3 ++ [A.Newline] ++ s4 ++ [A.Newline] ++
                            s5 ++ [A.Newline] ++ s6 ++ [A.Newline] ++
                            s7 ++ [A.Newline] ++ s8 ++ [A.Newline] ++
                            s9 )

        in A.FuncDec { name   = "fe_mul",
                       params = [h, f, g],
                       rtype  = Nothing,
                       body   = body' }

    ----------------------------------------------------------------------------
    -------------------------------- SQUARING ----------------------------------
    ----------------------------------------------------------------------------

    genSquareCoeffs' :: [A.Exp]
    genSquareCoeffs' = [A.Newline]

    genSquareOffsets' :: [A.Exp]
    genSquareOffsets' = [A.Newline]

    genSquareMul' :: [A.Exp]
    genSquareMul' = [A.Newline]

    genSquareSums' :: [A.Exp]
    genSquareSums' = [A.Newline]

    genSquareCarries' :: [A.Exp]
    genSquareCarries' = [A.Newline]

    genSquare ::  P.Params -> A.Dec
    genSquare p =
        let h = A.Param { pvar="h", ptyp="fe" }
            f = A.Param { pvar="f", ptyp="fe" }

            h' = A.Var { v="h", idx=Nothing, typ=Nothing }
            f' = A.Var { v="f", idx=Nothing, typ=Nothing }

            carry = A.Var {v="carry", idx=Nothing, typ=Just type32}
            numWords = len p

            s1 = genSimpleAssign numWords (VarX f') (ParamX f)
            s2 = genSquareCoeffs'
            s3 = genSquareOffsets'
            s4 = genSquareMul'
            s5 = genSquareSums'
            s6 = genVarDecs' numWords carry
            s7 = genSquareCarries'
            s8 = genSimpleAssign numWords (ParamX h) (VarX h')

            body' = A.Seq  ( s1 ++ [A.Newline] ++
                             s2 ++ [A.Newline] ++
                             s3 ++ [A.Newline] ++
                             s4 ++ [A.Newline] ++
                             s5 ++ [A.Newline] ++
                             s6 ++ [A.Newline] ++
                             s7 ++ [A.Newline] ++
                             s8 )

        in A.FuncDec { name   ="fe_sq",
                       params =[h, f],
                       rtype  =Nothing,
                       body   =body' }

    ----------------------------------------------------------------------------
    ------------------------------ INVERTING -----------------------------------
    ----------------------------------------------------------------------------
    -- source code: https://github.com/jedisct1/libsodium/blob/master/src/libsodium/crypto_scalarmult/curve25519/ref10/fe_invert_curve25519_ref10.c
    -- TODO there seems to be some things missing...

    genInvert :: P.Params -> A.Dec
    genInvert p =
        let out = A.Param { pvar="out", ptyp="fe"}
            z = A.Param {pvar="z", ptyp="fe"}

            body' = A.IntExp 0

        in A.FuncDec { name  ="fe_invert",
                       params=[out, z],
                       rtype =Nothing,
                       body  =body' }


    ----------------------------------------------------------------------------
    ----------------------------- HELPER FUNCTIONS -----------------------------
    ----------------------------------------------------------------------------

    genSimpleAssign :: Int -> A.GenVar -> A.GenVar -> [A.Exp]
    genSimpleAssign 0 _ _ = []
    genSimpleAssign n v1 v2 =
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
            assign = A.Assign { var=var',
                                val=A.VarExp (val'),
                                op=Nothing }
        in genSimpleAssign (n - 1) v1 v2 ++ [assign]


    genOper :: Int -> A.GenVar -> A.GenVar -> A.GenVar -> A.Op -> [A.Exp]
    genOper 0 _ _ _ _  = []
    genOper n v1 v2 v3 oper' =
        let id    = case v1 of
                    ParamX (A.Param {pvar=asdf, ptyp=paramtype}) -> Just (show (n - 1))
                    _ -> Nothing
            vtyp  = case v1 of
                    VarX (A.Var {v=v1, idx=i, typ=t}) -> t
                    _ -> Nothing
            vname = case v1 of
                    ParamX (A.Param {pvar=v1name, ptyp=typ}) -> v1name
                    VarX (A.Var {v=v1n, idx=i, typ=t})-> v1n ++ show (n - 1)
            v2'   = case v2 of
                    VarX (A.Var {v=v2name, typ=t}) -> v2name
                    ParamX (A.Param {pvar=v2n, ptyp=typ}) -> v2n
            v3'   = case v3 of
                    VarX (A.Var {v=v3name, typ=t}) -> v3name
                    ParamX (A.Param {pvar=v3n, ptyp=typ}) -> v3n

            var' = A.Var { v=vname, idx=id, typ=vtyp}
            val' = A.OpExp { left=A.VarExp A.Var{ v=v2' ++ show (n - 1),
                                                  idx=Nothing, typ=Nothing },
                             oper=oper',
                             right=A.VarExp A.Var{ v=v3' ++ show (n - 1),
                                                    idx=Nothing, typ=Nothing } }
            assign = A.Assign { var=var',
                                val=val',
                                op=Nothing }
        in genOper (n - 1) v1 v2 v3 oper' ++ [assign]

    genVarDecs' :: Int -> Var -> [A.Exp]
    genVarDecs' 0 _  = []
    genVarDecs' n v1 =
        let var' = A.Var { v = (v v1) ++ (show (n - 1)), typ=(typ v1), idx=Nothing}
        in genVarDecs' (n - 1) v1 ++ [A.VarDec var' ]


    genAll' :: Int -> A.Var -> A.Param -> A.Op -> [A.Exp]
    genAll' 0 _ _ _ = []
    genAll' n v1 v2 op' =
        let var' = A.Var {v=(v v1) ++ (show (n - 1)), idx=Nothing, typ=Nothing}
            val' = A.VarExp A.Var {v=pvar v2, idx=Nothing, typ=Nothing}
            assign = A.Assign { var=var', val=val', op=Just op' }
        in genAll' (n - 1) v1 v2 op' ++ [assign]

    --- small helper function for extracting odd, even elements of lists
    oddEvens :: [Int] -> ([Int], [Int])
    oddEvens [] = ([], [])
    oddEvens [x] = ([x], [])
    oddEvens (x:y:xs) = (x:xp, y:yp) where (xp, yp) = oddEvens xs