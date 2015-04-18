-- codegen module

module Gen where

  import Absyn as A
  import Param as P
  import Data.List

  genAbsyn :: String -> A.Prog
  genAbsyn s = 
    let p = P.genParams s
    in A.Prog [ gen_Package p,
                gen_Imports,
                gen_Type p, 
                gen_feZero, 
                gen_feOne,
                gen_feAdd,
                gen_feSub,
                gen_feCopy,
                gen_feCMove,
                gen_load3,
                gen_load4,
                gen_feFromBytes p,
                gen_feToBytes p,
                gen_feIsNegative,
                gen_feIsNonZero,
                gen_feNeg,
                gen_feMul p,
                gen_feSquare p,
                gen_feSquare2 p,
                gen_feInvert p,
                gen_fePowN p,
                gen_feString ]

-----------------------------------
--                               --
--          easy stuff           --
--                               --
-----------------------------------

  gen_Package :: P.Params -> A.Dec
  gen_Package p = A.PackageDec ("ed" ++ (show (base p)) ++ (show (offset p)))

  gen_Imports :: A.Dec
  gen_Imports = A.ImportDec "fmt"

  gen_Type :: P.Params -> A.Dec
  gen_Type p = A.TypeDec {td="fieldElement", ty="int32", size=(len p)}

  gen_feZero :: A.Dec
  gen_feZero = 
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

  gen_feOne :: A.Dec
  gen_feOne = 
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

  gen_feAdd :: A.Dec
  gen_feAdd =
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

  gen_feSub :: A.Dec
  gen_feSub =
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

  gen_feCopy :: A.Dec
  gen_feCopy =
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

  gen_feCMove :: A.Dec
  gen_feCMove =
    let param1 = A.Param { pvar="f", ptyp=Nothing }
        param2 = A.Param { pvar="g", ptyp=Just "*fieldElement" }
        param3 = A.Param { pvar="b", ptyp=Just "int32" }
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

-- super unclean code by what can you do eh
  gen_load' :: [A.Exp]
  gen_load' =
    let v = A.Var {v="r", idx=Nothing}
        exp1 = A.VarDecExp {vd=[v],
                            typ="int64"}
        exp2 = A.AssignExp {var=v,
                            aexp=A.TypeCastExp { 
                              tcexp=A.VarExp A.Var{ v="in", idx=Just "0"},
                              tctyp="int64" },
                            aoper = Nothing }
        exp3 = A.OpExp {left=A.TypeCastExp{ tcexp=A.VarExp A.Var{ v="in", 
                                                                  idx=Just "1"},
                                            tctyp="int64" }, 
                        oper=A.LShiftOp,
                        right=A.IntExp(8)}
        exp4 = A.AssignExp {var=v,
                            aexp=exp3,
                            aoper=Just A.OrOp }
        exp5 = A.OpExp {left=A.TypeCastExp{ tcexp=A.VarExp A.Var{ v="in", 
                                                                  idx=Just "2"},
                                            tctyp="int64" }, 
                        oper=A.LShiftOp,
                        right=A.IntExp(16)}
        exp6 = A.AssignExp {var=v,
                            aexp=exp5,
                            aoper=Just A.OrOp }
    in [exp1, exp2, exp4, exp6]

  gen_load3 :: A.Dec
  gen_load3 =
    let exps = gen_load'
        r = A.ReturnExp (A.VarExp A.Var {v="r", idx=Nothing})
        b = A.SeqExp (exps ++ [r])
    in A.FunctionDec  { fd="load3",
                        params=[A.Param{pvar="in", ptyp=Just "[]byte"}],
                        result=Just "int64",
                        body=b }

  gen_load4 :: A.Dec
  gen_load4 =
    let v = A.Var {v="r", idx=Nothing}
        exps = gen_load'
        exp1 = A.OpExp {left=A.TypeCastExp{ tcexp=A.VarExp A.Var{ v="in", 
                                                                  idx=Just "3"},
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

  gen_feIsNegative :: A.Dec
  gen_feIsNegative =
    let param = A.Param { pvar="f", ptyp=Just "*fieldElement" }
        e1 = A.VarDecExp {vd=[A.Var{v="s", idx=Nothing }],
                          typ="[32]byte" }
        e2 = A.AppExp { func="feToBytes",
                        args=[A.Var{v="&s", idx=Nothing },
                              A.Var{v="f", idx=Nothing }] }
        e3 = A.VarExp A.Var { v="s", idx=Just "0"}
        e4 = A.OpExp {left=e3,
                      oper=A.AndOp,
                      right=A.IntExp(1) }

        e5 = A.ReturnExp e4
    in A.FunctionDec  { fd="feIsNegative",
                        params=[param],
                        result=Just "byte",
                        body=A.SeqExp[e1, e2, e5] }

  gen_feIsNonZero :: A.Dec
  gen_feIsNonZero =
    let param = A.Param { pvar="f", ptyp=Just "*fieldElement" }
        e1 = A.VarDecExp {vd=[A.Var{v="s", idx=Nothing }],
                          typ="[32]byte" }
        e2 = A.AppExp { func="feToBytes",
                        args=[A.Var{v="&s", idx=Nothing },
                              A.Var{v="f", idx=Nothing }] }
        e3 = A.VarDecExp {vd=[A.Var{v="x", idx=Nothing }],
                          typ="uint8" }
        e4' = A.AssignExp { var=A.Var{v="x", idx=Nothing },
                            aexp=A.VarExp A.Var{v="b", idx=Nothing },
                            aoper=Just A.OrOp}
        e5 = A.RangeExp  { rvar=A.Var{v="_, b", idx=Nothing },
                              rangevar=A.Var{v="x", idx=Nothing },
                              rloop=e4' }

        g a b = A.OpExp { left=A.VarExp A.Var{v=a, idx=Nothing },
                          oper=A.RShiftOp,
                          right=A.IntExp(b)}
        g' a b = A.AssignExp {var=A.Var{v=a, idx=Nothing },
                              aexp=b,
                              aoper=Just A.OrOp}
        g'' a b = g' a (g a b)
        e6 = g'' "x" 4
        e7 = g'' "x" 2
        e8 = g'' "x" 1

        e9' = A.OpExp { left=A.VarExp A.Var {v="x", idx=Nothing },
                        oper=A.AndOp,
                        right=A.IntExp(1) }
        e10 = A.ReturnExp A.TypeCastExp { tcexp=e9', tctyp="int32" }
    in A.FunctionDec  { fd="feIsNonZero",
                        params=[param],
                        result=Just "int32",
                        body=A.SeqExp[e1, e2, e3, e5, e6, e7, e8, e10]}

  gen_feNeg :: A.Dec
  gen_feNeg =
    let param1 = A.Param { pvar="f", ptyp=Nothing }
        param2 = A.Param { pvar="g", ptyp=Just "*fieldElement" }
        var1 = A.Var {v="h", idx=Just "i"}
        var2 = A.Var {v="f", idx=Just "i"}
        exp = A.AssignExp { var=var1,
                            aexp=A.MinusExp(var2),
                            aoper=Nothing}
        body' = A.RangeExp  { rvar=A.Var{v="i", idx=Nothing},
                              rangevar=A.Var{v="h", idx=Nothing},
                              rloop=exp }
    in A.FunctionDec  { fd="feNeg",
                        params=[param1, param2],
                        result=Nothing,
                        body=body' }
  
  gen_feString :: A.Dec
  gen_feString =
    let v = A.NewLineExp
    in A.FunctionDec  { fd="(fe *fieldElement) String",
                        params=[],
                        result=Just "string",
                        body=v}

-----------------------------------
--                               --
--        gen_feFromBytes        --
--                               --
-----------------------------------

  gen_feFromBytes :: P.Params -> A.Dec
  gen_feFromBytes p =
    let le = len p
        param1 = A.Param { pvar="dst", ptyp=Just "*fieldElement" }
        param2 = A.Param { pvar="src", ptyp=Just "[]byte" }
        s1 = gen_feFromBytes_loadPattern p
        s2 = A.VarDecExp {vd=[A.Var{v="carry", idx=Nothing}], 
                          typ="[" ++ show le ++ "]int64"}
        v1 = s1 ++ [A.NewLineExp, s2, A.NewLineExp]

        l = chop (le - 1) ((rep p) ++ (rep p))
        n = chop (le - 1) ([0..le-1] ++ [0..le-1])
        l1 = accumOdds le l
        n1 = accumOdds le n 
        l2 = accumEvens le l
        n2 = accumEvens le n
        s3 = (gen_feFromBytes_modPattern l1 n1 (offset p) (sign p)) 
                ++ [A.NewLineExp]
                ++ (gen_feFromBytes_modPattern l2 n2 0 (sign p))
                ++ [A.NewLineExp]
        v2 = v1 ++ s3 ++ (gen_feFromBytes_save le)

    in A.FunctionDec {fd = "feFromBytes",
                      params=[param1, param2],
                      result=Nothing,
                      body=A.SeqExp(v2) }

-- TODO: figure out an optimal load pattern
  gen_feFromBytes_loadPattern :: Params -> [A.Exp]
  gen_feFromBytes_loadPattern p =
    let a1 = [4, 3, 3, 3 ,3, 4, 3, 3, 3, 3]
     in 
        gen_feFromBytes_loadPattern' 0 0 a1 (scanl1 (+) (0:(rep p)))

-- implements a loading scheme of 32 bits -> our representation
  -- f is the number of the variable h i.e. h0 or h1
  -- p is how many bytes have been loaded i.e. src[4:] or src[7:]
  -- x is the load pattern (from gen_feFromBytes_loadPattern)
  -- y is the cumulative size pattern (from params)
  gen_feFromBytes_loadPattern' :: Int -> Int -> [Int] -> [Int] -> [A.Exp]
  gen_feFromBytes_loadPattern' f p (x:xs) (y:ys) = 
    let s   = 8 * p - y
        p'  = case p of
                0 -> ""
                _ -> show p
        i   = A.AppExp {func="load" ++ show x,
                        args=[A.Var{v="src", idx=Just (p' ++ ":")}]}
        i'  = case xs of
                [] -> A.ParenExp A.OpExp {left=i,
                                          oper=A.AndOp,
                                          right=A.IntExp(2^(x*8-1)-1)}
                _ -> i
        i'' = case s of
                0 -> i'
                _ -> A.OpExp {left=i',
                              oper=A.LShiftOp,
                              right=A.IntExp(s)}
        exp = A.AssignExp { var=A.Var {v="h" ++ show f, idx=Nothing},
                            aexp=i'',
                            aoper=Nothing }
    in
      exp : (gen_feFromBytes_loadPattern' (f+1) (p+x) xs ys)
  gen_feFromBytes_loadPattern' f p [] [y] = []
  gen_feFromBytes_loadPattern' _ _ _ _ = error ("error in gen_feFromBytes_loadPattern: "
      ++ "length of size pattern does not match length of load pattern")

-- reduces the loaded pattern into our representation
--
-- when called with a non-zero offset
-- it is the wrap-around carry from max to 0 bucket for reducing modulo prime
--
-- when called with zero offset
-- it is just regular spillover into next bucket

  -- xs is the load pattern
  -- ys is the indices
  gen_feFromBytes_modPattern :: [Int] -> [Int] -> Int -> P.Sign -> [A.Exp]
  gen_feFromBytes_modPattern (x:xs) (y:ys) o s =
    let h  = A.Var{v="h" ++ (show y), idx=Nothing}
        h' = case o of
                0 -> A.Var{v=("h" ++ (show (y+1))), idx=Nothing}
                _ -> A.Var{v="h0", idx=Nothing}
        v  = A.Var {v="carry", idx=Just (show y)}
        e1 = A.OpExp {left=A.IntExp(1),
                      oper=A.LShiftOp,
                      right=A.IntExp(x-1) }
        e2 = A.ParenExp A.OpExp { left=A.VarExp(h),
                                  oper=A.PlusOp,
                                  right=e1 }
        e3 = A.OpExp {left=e2,
                      oper=A.RShiftOp,
                      right=A.IntExp(x)}
        e4 = A.AssignExp {var=v,
                          aexp = e3,
                          aoper=Nothing}
        f1 = A.OpExp {left=A.VarExp(v),
                      oper=A.TimesOp,
                      right=A.IntExp(o)}
        f2 = case o of
              0 -> A.AssignExp {var=h',
                          aexp=A.VarExp(v),
                          aoper=Just A.PlusOp }
              _ -> A.AssignExp {var=h',
                                aexp=f1,
                                aoper=Just A.PlusOp }
        g1 = A.OpExp {left=A.VarExp(v),
                      oper=A.LShiftOp,
                      right=A.IntExp(x)}
        g2 = A.AssignExp {var=h,
                          aexp=g1,
                          aoper=Just A.MinusOp}
    in [e4, f2, g2] ++ (gen_feFromBytes_modPattern xs ys 0 s)
  gen_feFromBytes_modPattern [] [] _ _ = []
  gen_feFromBytes_modPattern _ _ _ _ =
    error ("error in gen_feFromBytes_modPattern: size mismatch")

  gen_feFromBytes_save :: Int -> [A.Exp]
  gen_feFromBytes_save len =
    gen_feFromBytes_save' len len

  gen_feFromBytes_save' :: Int -> Int -> [A.Exp]
  gen_feFromBytes_save' len 0 = []
  gen_feFromBytes_save' len cur =
    if (cur < 0)
    then error ("error in gen_feFromBytes_save: negative length?")
    else  let cur' = len - cur
              a = A.TypeCastExp { tcexp=A.VarExp A.Var{ v=("h"++(show cur')),
                                                        idx=Nothing },
                                  tctyp="int32" }
              e = A.AssignExp { var=A.Var{v="dst", idx=Just (show cur')},
                                aexp=a,
                                aoper=Nothing}
          in e : (gen_feFromBytes_save' len (cur-1))

-----------------------------------
--                               --
--         gen_feToBytes         --
--                               --
-----------------------------------

-- TODO
-- There's a problem here, in that the ed22519 code I've been
-- modelling after actually uses a pretty strong condition
-- to marshall a fieldelement back to a byte array.
  gen_feToBytes :: P.Params -> A.Dec
  gen_feToBytes p =
    let param1 = A.Param { pvar="s", ptyp=Just "*[32]byte" }
        param2 = A.Param { pvar="h", ptyp=Just "*fieldElement" }
        v = A.NewLineExp
    in A.FunctionDec {fd = "feToBytes",
                      params=[param1, param2],
                      result=Nothing,
                      body=v }

-----------------------------------
--                               --
--           gen_feMul           --
--                               --
-----------------------------------

-- TODO: generate carry logic
--
-- Assertions: the schoolbook multiplication must not "underflow"
-- That is, we have the following situation:
--         f3     f2     f1     f0
--      x  g3     g2     g1     g0
--      --------------------------
--       f3g0   f2g0   f1g0   f0g0
--       f2g1   f1g1   f0g1   f3g1
--       f1g2   f0g2   f3g2   f2g2
--       f0g3   f3g3   f2g3   f1g3
-- (The modulus is assumed to be some perfect power of 2 to simplify
-- the notation). Then we assume the exponent of all the products in the
-- sum for a particular block is always at least the exponent of that
-- particular block.
--
-- This is actually pretty hard to guarantee. We have some upper bound as a
-- power of two of the fieldelement size s (for example, 255 in the case
-- of ed25519), and some number of blocks n (10 for ed25519).
--
-- Now we have that our blocks are indexed from zero, it is clear that in
-- the kth block of the product, it consists exactly of all products ai * bj
-- such that i + j = k (mod n). Then let ei denote the exponent on the ith
-- block, this is basically a claim that for all (i,j) such that (i + j) = k
-- (mod n), ei + ej >= ek (mod s). I haven't really found a way to
-- systematically turn a prime into such a representation yet.
--
-- The implementation plan for this (and subsequent arithmetic operations)
-- is to just generate all the necessary pairwise products as detailed above
-- and then afterward go through and delete/consolidate/insert precalculations
-- in an optimization step.
-- 
-- The optimizations will consist of two flavors. First, we lift the offset to
-- g. What I mean is that when we do the modulus wrap-around, we will need to
-- multiply by the offset. Thus if we premultiply all the elements of g by
-- the offset in advance we can just use those values.
--
-- Second, we will "equalize" the representation by precalculating f. What I
-- mean here is that in certain cases, as alluded to by the discussion above,
-- our term ai * bj is actually represented by ai * bj * (2^(ek - ei - ej)).
-- If the representation has been chosen correctly, this is always positive.
-- and if chose well we can hope it is always 0 or 1. Then we can just
-- precompute this extra padding by lifting it to the terms in f where
-- it shows up.
-- 
-- We may precompute some things we don't ever actually need. We would rather
-- this be the case, that way if we ever need the term f1 * g2 * offset * 4, 
-- we can just refer to the variable f1_4 * g2_{offset} without worrying about
-- whether it exists. A post-optimization step will then go through and delete
-- the unneeded precomputations by looking for unused variables in the itree.
             
  gen_feMul :: P.Params -> A.Dec
  gen_feMul p =
    let var_out = "h"
        var_in1 = "f"
        var_in2 = "g"
        param1  = A.Param { pvar=var_out, ptyp=Nothing }
        param2  = A.Param { pvar=var_in1, ptyp=Nothing }
        param3  = A.Param { pvar=var_in2, ptyp=Just "*fieldElement" }
        v1      = reverse (gen_feMul_load var_in1 (len p))
        v2      = reverse (gen_feMul_load var_in2 (len p))
        v3      = reverse (gen_feMul_precomp_offset var_in2 (offset p) (len p))
        os      = reverse (scanr1 (+) (rep p))
        os'     = [n - (os !! 0) | n <- os]
        a       = gen_feMul_repeq (base p) os' os' (os' ++ os')
        a'      = reverse (gen_feMul_reindex a)
        v4      = gen_feMul_precomp_rep var_in1 a 0
        v5      = gen_feMul_precomp_pairs var_in1 var_in2 0 (len p) (offset p) a
        v6      = gen_feMul_sums var_out 0 (len p) (offset p) var_in1 var_in2 a'
        v7      = A.VarDecExp { vd=[A.Var {v="carry", idx=Nothing}], 
                                typ=("[" ++ show (len p) ++ "]int64") }
        v8      = gen_feMul_carry
        v9      = reverse (gen_feMul_save var_out (len p))
    in A.FunctionDec {fd = "feMul",
                      params=[param1, param2, param3],
                      result=Nothing,
                      body=A.SeqExp(v1 ++ v2 ++ v3 ++ v4 ++ v5 ++ v6 
                                       ++ [v7] ++ v8 ++ v9) }

  gen_feMul_load :: String -> Int -> [A.Exp]
  gen_feMul_load _ 0 = []
  gen_feMul_load var len = 
    let p     = gen_feMul_load var (len-1)
        r     = A.VarExp A.Var { v=var, idx=Just (show (len-1)) }
        lval  = A.Var { v=(var ++ (show (len-1))), idx=Nothing }
        exp   = A.InitExp { ivar=lval,
                            iexp=r }
    in exp : p

  gen_feMul_precomp_offset :: String -> Int -> Int -> [A.Exp]
  gen_feMul_precomp_offset _ _ 1 = []
  gen_feMul_precomp_offset var offset len =
    let p     = gen_feMul_precomp_offset var offset (len-1)
        lval  = A.Var { v=(var ++ (show (len-1)) ++ "_" ++ (show (offset))), 
                        idx=Nothing }
        v     = A.Var { v=(var ++ (show (len-1))), idx=Nothing }
        r     = A.OpExp { left=A.IntExp(offset),
                          oper=A.TimesOp,
                          right=A.VarExp v }
        exp   = A.InitExp { ivar=lval,
                            iexp=r }
    in exp : p

-- This returns a list of lists. The first index i corresponds to fi, the 
-- second index j corresponds to gj, and the entry at (i,j) is (ei + ej - ek),
-- where k = i + j (mod s)
  gen_feMul_repeq :: Int -> [Int] -> [Int] -> [Int] -> [[Int]]
  gen_feMul_repeq m [] _ _ = []
  gen_feMul_repeq m (i:is) js (k:ks) =
    let h = gen_feMul_repeq' m i js (k:ks)
        t = gen_feMul_repeq m is js ks
    in h : t

  gen_feMul_repeq' :: Int -> Int -> [Int] -> [Int] -> [Int]
  gen_feMul_repeq' _ _ [] _ = []
  gen_feMul_repeq' m i (j:js) (k:ks) =
    let h = ((i + j) `mod` m) - k
        t = gen_feMul_repeq' m i js ks
     in h : t

-- Takes a list of lists and reindexes, i.e. [[1,2],[3,4]] -> [[1,3],[2,4]]
  gen_feMul_reindex :: [[Int]] -> [[Int]]
  gen_feMul_reindex a =
    let x = map (splitAt 1) a
        d = concat (map fst x)
        e = map snd x
    in if (d == [])
       then []
       else d : (gen_feMul_reindex e)

  gen_feMul_precomp_rep :: String -> [[Int]] -> Int -> [A.Exp]
  gen_feMul_precomp_rep _ [] _ = []
  gen_feMul_precomp_rep v (i:is) idx =
    let i' = nub i
        t  = gen_feMul_precomp_rep v is (idx + 1)
        v' = v ++ (show idx)
        h  = gen_feMul_precomp_rep' v' i'
     in h ++ t

  gen_feMul_precomp_rep' :: String -> [Int] -> [A.Exp]
  gen_feMul_precomp_rep' _ [] = []
  gen_feMul_precomp_rep' v' (i:is) = 
    let t   = gen_feMul_precomp_rep' v' is
    in if (i == 0)
       then t
       else let v'' = v' ++ "_" ++ (show (2^i))
                r   = A.OpExp {right=A.VarExp A.Var{ v=v', idx=Nothing },
                      oper=TimesOp,
                      left=A.IntExp(2^i) }
                l   = A.Var { v=v'', idx=Nothing }
                h   = A.InitExp { ivar=l,
                                  iexp=r }
        
            in h : t

  gen_feMul_precomp_pairs :: String -> String -> Int -> Int -> Int -> [[Int]] -> [A.Exp]
  gen_feMul_precomp_pairs _ _ _ _ _ [] = []
  gen_feMul_precomp_pairs in1 in2 idx len offset (a:as) = 
    let h    = gen_feMul_precomp_pairs' in1 in2 idx 0 len offset a
        t    = gen_feMul_precomp_pairs in1 in2 (idx+1) len offset as
    in h ++ t

  gen_feMul_precomp_pairs' :: String -> String -> Int -> Int -> Int -> Int -> [Int] -> [A.Exp]
  gen_feMul_precomp_pairs' _ _ _ _ _ _ [] = []
  gen_feMul_precomp_pairs' in1 in2 in1idx in2idx len offset (a:as) =
    let in1'          = in1 ++ (show in1idx)
        in2'          = in2 ++ (show in2idx)
        (mult2, var2) = if (in1idx + in2idx >= len)
                        then (offset, in2' ++ "_" ++ (show offset))
                        else (1, in2')
        (mult1, var1) = if (a == 0)
                        then (1, in1')
                        else (2^a, in1' ++ "_" ++ (show (2^a)))
        mult          = if (mult1 * mult2 == 1)
                        then ""
                        else ("_" ++ (show (mult1 * mult2)))
        lval          = in1' ++ in2' ++ mult
        e1            = A.TypeCastExp { tcexp=A.VarExp 
                                              A.Var { v=var1, idx=Nothing},
                                        tctyp="int64" }
        e2            = A.TypeCastExp { tcexp=A.VarExp 
                                              A.Var { v=var2, idx=Nothing},
                                        tctyp="int64" }
        e             = A.OpExp { left=e1,
                                  oper=A.TimesOp,
                                  right=e2 }
        h             = A.InitExp { ivar=A.Var { v=lval, idx=Nothing },
                                    iexp=e }
        t             = gen_feMul_precomp_pairs' in1 in2 in1idx (in2idx+1) len offset as
    in h : t

  gen_feMul_sums :: String -> Int -> Int -> Int -> String -> String -> [[Int]] -> [A.Exp]
  gen_feMul_sums _ _ _ _ _ _ [] = []
  gen_feMul_sums out outidx len offset in1 in2 (a:as) = 
    if (outidx >= len)
    then []
    else let t  = gen_feMul_sums out (outidx+1) len offset in1 in2 as
             a' = drop outidx (a++a)
             h  = gen_feMul_sums' (out ++ (show outidx)) outidx len offset in1 in2 a'
         in h : t
  
  gen_feMul_sums' :: String -> Int -> Int -> Int -> String -> String -> [Int] -> A.Exp
  gen_feMul_sums' out outidx len offset in1 in2 a =
    let lval = A.Var { v=out, idx=Nothing }
        e    = gen_feMul_sums'' len offset in1 0 in2 outidx a
     in A.InitExp { ivar=lval, iexp=e }

  gen_feMul_sums'' :: Int -> Int -> String -> Int -> String -> Int -> [Int] -> A.Exp
  gen_feMul_sums'' _ _ _ _ _ _ [] = A.NewLineExp
  gen_feMul_sums'' len offset in1 in1idx in2 in2idx (a:as) =
    let in2idx' = (in2idx + len) `mod` len
        s       = if (in2idx < 0)
                  then offset
                  else 1
        a'      = s * (2^a)
        sub     = if (a' == 1)
                  then ""
                  else "_" ++ (show a')
        v'      = in1 ++ (show in1idx) ++ in2 ++ (show in2idx') ++ sub
        h       = A.VarExp A.Var { v=v', idx=Nothing }
    in if (len <= (in1idx + 1))
       then h
       else let t = gen_feMul_sums'' len offset in1 (in1idx+1) in2 (in2idx-1) as
            in A.OpExp { left=h,
                         oper=A.PlusOp, 
                         right=t }

  gen_feMul_carry :: [A.Exp]
  gen_feMul_carry = [A.NewLineExp]

  gen_feMul_save :: String -> Int -> [A.Exp]
  gen_feMul_save _ 0 = []
  gen_feMul_save out len = 
    let lval  = A.Var { v=out, idx=Just (show (len-1)) }
        e     = A.TypeCastExp { tcexp=A.VarExp A.Var { v=out ++ (show (len-1)),
                                                       idx=Nothing },
                                tctyp="int32" }
        h     = A.AssignExp{ var=lval, aexp=e, aoper=Nothing }
        t     = gen_feMul_save out (len-1)
     in h : t

-----------------------------------
--                               --
--          gen_feSquare         --
--                               --
-----------------------------------

-- TODO
  gen_feSquare :: P.Params -> A.Dec
  gen_feSquare p =
    let param1 = A.Param { pvar="h", ptyp=Nothing }
        param2 = A.Param { pvar="f", ptyp=Just "*fieldElement" }
        v = A.NewLineExp
    in A.FunctionDec {fd = "feSquare",
                      params=[param1, param2],
                      result=Nothing,
                      body=v }

-----------------------------------
--                               --
--         gen_feSquare2         --
--                               --
-----------------------------------

-- TODO
  gen_feSquare2 :: P.Params -> A.Dec
  gen_feSquare2 p =
    let param1 = A.Param { pvar="h", ptyp=Nothing }
        param2 = A.Param { pvar="f", ptyp=Just "*fieldElement" }
        v = A.NewLineExp
    in A.FunctionDec {fd = "feSquare2",
                      params=[param1, param2],
                      result=Nothing,
                      body=v }

-----------------------------------
--                               --
--          gen_feInvert         --
--                               --
-----------------------------------

-- TODO
  gen_feInvert :: P.Params -> A.Dec
  gen_feInvert p =
    let param1 = A.Param { pvar="out", ptyp=Nothing }
        param2 = A.Param { pvar="z", ptyp=Just "*fieldElement" }
        v = A.NewLineExp
    in A.FunctionDec {fd = "feInvert",
                      params=[param1, param2],
                      result=Nothing,
                      body=v }

-----------------------------------
--                               --
--           gen_fePowN          --
--                               --
-----------------------------------

-- TODO what is this?
  gen_fePowN :: P.Params -> A.Dec
  gen_fePowN p =
    let param1 = A.Param { pvar="out", ptyp=Nothing }
        param2 = A.Param { pvar="z", ptyp=Just "*fieldElement" }
        v = A.NewLineExp
    in A.FunctionDec {fd = "fePowN",
                      params=[param1, param2],
                      result=Nothing,
                      body=v }

-----------------------------------
--                               --
--            helpers            --
--                               --
-----------------------------------

  -- chops off the first Int elements of list [a]
  chop :: Int -> [a] -> [a]
  chop 0 xs = xs
  chop n (x:xs) = chop (n-1) xs
  chop _ [] = error "requested chop is longer than length of list"

  -- gets the odd terms in the first len elements of list [a]
  accumOdds :: Int -> [a] -> [a]
  accumOdds 0 xs = []
  accumOdds n (x:xs) = x : (accumEvens (n-1) xs)

  -- gets the even terms in the first len elements of list [a]
  accumEvens :: Int -> [a] -> [a]
  accumEvens 0 xs = []
  accumEvens n (x:xs) = accumOdds (n-1) xs

