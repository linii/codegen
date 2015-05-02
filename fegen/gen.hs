-- codegen module

module Gen where

  import Absyn as A
  import Param as P
  import Opt as O
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

        l = drop (le - 1) ((rep p) ++ (rep p))
        n = drop (le - 1) ([0..le-1] ++ [0..le-1])
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
--       need to guarantee that the precomputations fit into int32s
--       should probably adapt feSquare's code for this - more general
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
        a'      = reverse (a)
        v4      = gen_feMul_precomp_rep var_in1 a 0
        v5      = gen_feMul_precomp_pairs var_in1 var_in2 0 (len p) (offset p) a
        v5'     = O.schedule (opt p) v5
        v6      = gen_feMul_sums var_out 0 (len p) (offset p) var_in1 var_in2 a'
        v7      = A.VarDecExp { vd=[A.Var {v="carry", idx=Nothing}], 
                                typ=("[" ++ show (len p) ++ "]int64") }
        v8      = gen_feMul_carry
        v9      = reverse (gen_feMul_save var_out (len p))
        v       = O.prune (opt p) [v1, v2, v3, v4, v5', v6, [v7], v8, v9] [param1, param2, param3]
    in A.FunctionDec {fd = "feMul",
                      params=[param1, param2, param3],
                      result=Nothing,
                      body=A.SeqExp v }

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

-- TODO: generate carry logic
--       check that all the precomputations still fit in int32s
--
-- Finding the correct preoptimizations is a little less straightforward
-- here than previously. 
--
-- First, we calculate all the coefficients needed in the sum. 
-- Second, we find the minimum coefficient for a given term that includes 
--   an offset (for example, if the offset is 19, but every term including 
--   f1 * 19 has minimum coefficient f1 * 38, then we don't need f1 * 19, 
--   and can actually just go straight to f1 * 38). We only need to do this 
--   for the last ceil(s/2) elements. 
-- Finally, we go through and find all remaining precomputations. This should
--   give one array of offset-related precomputations, and one array of 
--   coefficients, and to produce the final sums we can just divide the
--   corresponding term of the coefficients by the offset terms and the result
--   should have been precomputed.
--
-- For example, let's say our sum is h[0] = f0f0   + 2*19*f1f3 + 19*f2f2.
--                                   h[1] = 2*f0f1 + 19*f2f3
-- On the first sweep we only are looking for terms containing offset = 19.
-- Here the length s of the representation is 4, so we only need to look for
-- the top ceil(4/2)=2 terms, which are f2 and f3. f3 has a 2*19 term and a
-- 19 term, so we precompute f3_19. f2 has a 19 term, so we precompute 
-- f2_19. Now we go through and find that a 2*f1 term, finishing off all the
-- coefficients so we precompute that.
-- That yields h[0] = f0 * f0 + f1_2 * f3_19 + f2 * f2_19
--             h[1] = f0 * f1_2 + f2_19 * f3
--
-- Note that in the last step, we have to be a little careful - the algorithm
-- might take several steps to finish. Imagine we have a 4*f2*f1 term. If
-- we've already precomputed 2*f1 and 2*f2, then we should be done. Thus
-- we do a first-pass computation to find the absolute minimum needed for
-- each term, the build a tuple of pairs, precomputations, and sums
-- on the fly.
--
-- Heuristically this actually does pretty well in finding a small set of
-- precomputations.
  gen_feSquare :: P.Params -> A.Dec
  gen_feSquare p =
    let var_in       = "f"
        var_out      = "h"
        param1       = A.Param { pvar=var_out, ptyp=Nothing }
        param2       = A.Param { pvar=var_in, ptyp=Just "*fieldElement" }
        v1           = reverse (gen_feMul_load var_in (len p))
        os           = reverse (scanr1 (+) (rep p))
        os'          = [n - (os !! 0) | n <- os]
        ra           = reverse (gen_feMul_repeq (base p) os' os' (os' ++ os'))
        ca           = gen_feSquare_coeff ra (len p) (offset p) 0
        moa          = gen_feSquare_min_off (gen_feSquare_reindex ca) 0 (len p)
        rca          = gen_feSquare_red_coeff ca moa
        pca          = gen_feSquare_precomp_coeff (gen_feSquare_reindex rca)
        init         = gen_feSquare_init (len p)
        (da, v4, v5) = gen_feSquare_sums ca moa pca init [] [] var_in (len p) 0
        v4'          = O.schedule (opt p) v4
        v2           = gen_feSquare_init_coeff var_in da 0
        v3           = gen_feSquare_init_off var_in moa 0
        v6           = A.VarDecExp { vd=[A.Var {v="carry", idx=Nothing}], 
                                     typ=("[" ++ show (len p) ++ "]int64") }
        v7           = reverse (gen_feMul_save var_out (len p))
        v            = O.prune (opt p) [v1, v2, v3, v4, (reverse v5), [v6], v7] [param1, param2]
    in A.FunctionDec {fd = "feSquare",
                      params=[param1, param2],
                      result=Nothing,
                      body=A.SeqExp v }

-- Generates the coefficients needed for h[i]
-- First index is by h, second index is by the first of the input pair
-- i.e. the first list is h[0] = a*f0*f0 + b*f1*f3 + c*f2*f2 + d*f3*f1
--                           -> [a,        0,        c,        d + b]
  gen_feSquare_coeff :: [[Int]] -> Int -> Int -> Int -> [[Int]]
  gen_feSquare_coeff [] _ _ _ = []
  gen_feSquare_coeff (a:as) len offset idx =
    let t = gen_feSquare_coeff as len offset (idx+1)
        h = gen_feSquare_coeff' a len offset idx 0
     in h : t

  gen_feSquare_coeff' :: [Int] -> Int -> Int -> Int -> Int -> [Int]
  gen_feSquare_coeff' [] _ _ _ _ = []
  gen_feSquare_coeff' (a:as) len offset idx i =
    let t    = gen_feSquare_coeff' as len offset idx (i+1)
        i' = (len + idx - i) `mod` len
        m    = if (i' == i)
               then 1
               else 2
        m'   = if (i' + i >= len)
               then offset
               else 1
        m''  = if (i' > i)
               then 0
               else 1
        h    = m * m' * m'' * (2^a)
    in h : t

-- Takes a list of lists and reindexes, i.e. [[1,2],[3,4]] -> [[1,3],[2,4]]
  gen_feSquare_reindex :: [[Int]] -> [[Int]]
  gen_feSquare_reindex a =
    let x = map (splitAt 1) a
        d = concat (map fst x)
        e = map snd x
    in if (d == [])
       then []
       else d : (gen_feSquare_reindex e)

-- Generates the minimum offsets. For example, if h9 shows up in the terms
-- h0h9, 2h1h9, 19*h1h9, 38*h2h9... then the list should have an entry of 19
-- for h9. 
-- You are also only "responsible" for terms in which you are the larger index.
-- So if the term is in 19*h8h9, h8 doesn't get the 19.
  gen_feSquare_min_off :: [[Int]] -> Int -> Int -> [Int]
  gen_feSquare_min_off [] _ _ = []
  gen_feSquare_min_off (a:as) i len = 
    let t = gen_feSquare_min_off as (i+1) len
        h = gen_feSquare_min_off' a 0 i len
    in h : t

  gen_feSquare_min_off' :: [Int] -> Int -> Int -> Int -> Int
  gen_feSquare_min_off' [] _ _ _ = 0
  gen_feSquare_min_off' (a:as) i idx len =
    let t = gen_feSquare_min_off' as (i+1) idx len
        h = if (i >= idx)
            then t
            else if (t > 0)
                 then min a t
                 else a
    in h

-- Generates the reduced set of coefficients, assuming we have the
-- preoptimizations from gen_feSquare_min_off.
  gen_feSquare_red_coeff :: [[Int]] -> [Int] -> [[Int]]
  gen_feSquare_red_coeff [] _ = []
  gen_feSquare_red_coeff (a:as) b = 
    let t = gen_feSquare_red_coeff as b
        h = gen_feSquare_red_coeff' a b
     in h : t
 
  gen_feSquare_red_coeff' :: [Int] -> [Int] -> [Int]
  gen_feSquare_red_coeff' [] [] = []
  gen_feSquare_red_coeff' (a:as) (b:bs) =
    let t = gen_feSquare_red_coeff' as bs
        h = if ( (not (b == 0)) && (a `rem` b == 0))
            then a `div` b
            else a
     in h : t
  gen_feSquare_red_coeff' _ _ = error "Gen.gen_feSquare_red_coeff: length mismatch"

-- Generates a first-pass list of precomputations for each element
  gen_feSquare_precomp_coeff :: [[Int]] -> [Int]
  gen_feSquare_precomp_coeff [] = []
  gen_feSquare_precomp_coeff (a:as) =
    let a' = filter (>1) a
        h  = if (a' == [])
             then 0
             else minimum a'
        t  = gen_feSquare_precomp_coeff as
     in h : t


-- Generates the precomputations for the reduced set of coefficients.
  gen_feSquare_pc_coeff :: String -> [[Int]] -> Int -> [A.Exp]
  gen_feSquare_pc_coeff _ [] _ = []
  gen_feSquare_pc_coeff var_in (a:as) idx = 
    let a'      = nub a
        var_in' = var_in ++ (show idx)
        h       = gen_feSquare_pc_coeff' var_in' a'
        t       = gen_feSquare_pc_coeff var_in as (idx+1)
    in h ++ t
  
  gen_feSquare_pc_coeff' :: String -> [Int] -> [A.Exp]
  gen_feSquare_pc_coeff' _ [] = []
  gen_feSquare_pc_coeff' var_in (a:as) =
    if (a == 0)
    then gen_feSquare_pc_coeff' var_in as
    else let  var_in' = var_in ++ "_" ++ (show (2^a))
              e       = A.OpExp { left=A.IntExp(2^a),
                                  oper=A.TimesOp,
                                  right=A.VarExp A.Var { v=var_in, 
                                                         idx=Nothing } }
              lval    = A.Var { v=var_in', idx=Nothing }
              h       = A.InitExp { ivar = lval, iexp = e }
              t       = gen_feSquare_pc_coeff' var_in as
         in h : t

-- Generates the precomputations for the minimum offsets.
  gen_feSquare_pc_offset :: String -> [Int] -> Int -> [A.Exp]
  gen_feSquare_pc_offset _ [] _ = []
  gen_feSquare_pc_offset var_in (a:as) idx =
    if (a == 1)
    then gen_feSquare_pc_offset var_in as (idx+1)
    else let  var_in' = var_in ++ "_" ++ (show a)
              e       = A.OpExp { left=A.IntExp(a),
                                  oper=A.TimesOp,
                                  right=A.VarExp A.Var { v=var_in, 
                                                         idx=Nothing } }
              lval    = A.Var { v=var_in', idx=Nothing }
              h       = A.InitExp { ivar = lval, iexp = e }
              t       = gen_feSquare_pc_coeff' var_in as
         in h : t

-- makes an array length len of empty arrays
  gen_feSquare_init :: Int -> [[Int]]
  gen_feSquare_init 0 = []
  gen_feSquare_init a = [] : (gen_feSquare_init (a-1))

-- inputs: the array of coefficients indexed by h, second index the larger coeff
--         the array of precomputed offsets (order corresponds to i)
--         the array of minimum over i of h[j][i] / offset[i]
-- outputs: an array of precomputations needed, indexed by the block
--          an array of pair products
--          an array of the sums
  gen_feSquare_sums :: [[Int]] -> [Int] -> [Int] -> [[Int]] -> [A.Exp] 
    -> [A.Exp] -> String -> Int -> Int -> ([[Int]], [A.Exp], [A.Exp])
  gen_feSquare_sums [] _ _ a b c _ _ _ = (a,b,c)
  gen_feSquare_sums (a:as) offsets coeff precomp pairprod sums var_in len idx =
    let (precomp', pairprod', sum) = gen_feSquare_sums' a offsets coeff precomp pairprod [] 0 var_in len idx
        lval = "h" ++ (show idx)
        exp  = A.InitExp {ivar=A.Var { v=lval, idx=Nothing},
                          iexp=(A.SeqExp (sum)) }
    in gen_feSquare_sums as offsets coeff precomp' pairprod' (exp : sums) var_in len (idx+1)

-- Possibly the worst code I've written. I'm sorry.
  gen_feSquare_sums' :: [Int] -> [Int] -> [Int] -> [[Int]] -> [A.Exp] 
    -> [A.Exp] -> Int -> String -> Int -> Int -> ([[Int]], [A.Exp], [A.Exp])
  gen_feSquare_sums' [] _ _ a b c _ _ _ _ = (a,b,c)
  gen_feSquare_sums' (0:as) (o:os) (c:cs) ps pairprod sums i var_in len idx = 
    let (ps', pairprod', sums') = gen_feSquare_sums' as os cs ps pairprod sums (i+1) var_in len idx
     in (ps', pairprod', sums')
  gen_feSquare_sums' (a:as) (o:os) (c:cs) ps pairprod sums i var_in len idx = 
    let (ps', pairprod', sums') = gen_feSquare_sums' as os cs ps pairprod sums (i+1) var_in len idx
        i'   = (len + idx - i) `mod` len
    in if (i' > i)
       then (ps', pairprod', sums')
       else 
            if (i + i' >= len)
            then let 
                     off      = if (o > 1)
                                then "_" ++ (show o)
                                else ""
                     coef     = if (a > 1 && (a `div` o) > 1)
                                then "_" ++ (show (a `div` o))
                                else ""
                     c''      = if (a > 1)
                                then "_" ++ show(a)
                                else ""
                     ps''     = appendnth ps' i' (a `div` o)
                     l        = A.TypeCastExp { tcexp=A.VarExp A.Var {v=(var_in ++ (show i') ++ coef), idx=Nothing},
                                                tctyp="int64" }
                     r        = A.TypeCastExp { tcexp=A.VarExp A.Var {v=(var_in ++ (show i) ++ off), idx=Nothing},
                                                tctyp="int64" }
                     ppentry  = A.OpExp { left=l,
                                          oper=A.TimesOp,
                                          right=r }
                     ppentry' = A.InitExp { ivar = A.Var { v=(var_in ++ (show i') ++ var_in ++ (show i) ++ c''),
                                                           idx=Nothing},
                                            iexp = ppentry }
                     sumentry = A.VarExp A.Var { v=(var_in ++ (show i') ++ var_in ++ show(i) ++ c''),
                                                 idx=Nothing }
                     sumentry' = if (null sums')
                                 then sumentry
                                 else let e = head sums'
                                      in A.OpExp {left=sumentry,
                                                  oper=A.PlusOp,
                                                  right=e}
                 in (ps'' , (ppentry':pairprod') , [sumentry'])
            else if (a == 1)
                 then let l        = A.TypeCastExp { tcexp=A.VarExp A.Var {v=(var_in ++ (show i')), idx=Nothing },
                                                tctyp="int64" }
                          r        = A.TypeCastExp { tcexp=A.VarExp A.Var {v=(var_in ++ (show i)), idx=Nothing },
                                                tctyp="int64" }
                          ppentry  = A.OpExp { left=l,
                                              oper=A.TimesOp,
                                              right=r }
                          ppentry' = A.InitExp { ivar = A.Var { v=(var_in ++ (show i') ++ var_in ++ (show i)),
                                                                idx=Nothing},
                                                 iexp = ppentry }
                          sumentry = A.VarExp A.Var { v=(var_in ++ (show i') ++ var_in ++ show(i)),
                                                 idx=Nothing }
                          sumentry' = if (null sums')
                                      then sumentry
                                      else let e = head sums'
                                          in A.OpExp {left=sumentry,
                                                      oper=A.PlusOp,
                                                      right=e}
                      in (ps' , (ppentry':pairprod') , [sumentry'])
                 else let ps''         = appendnth ps' i (a `div` c)
                          ps'''        = appendnth ps'' i' c
                          c2           = if (a `div` c == 1)
                                         then ""
                                         else "_" ++ (show (a `div` c))
                          c1           = if (c == 1)
                                         then ""
                                         else "_" ++ (show c)
                          l        = A.TypeCastExp { tcexp=A.VarExp A.Var { v=(var_in ++ (show i') ++ c1), idx=Nothing },
                                                tctyp="int64" }
                          r        = A.TypeCastExp { tcexp=A.VarExp A.Var { v=(var_in ++ (show i) ++ c2), idx=Nothing },
                                                tctyp="int64" }
                          ppentry  = A.OpExp { left=l,
                                               oper=A.TimesOp,
                                               right=r }
                          ppentry' = A.InitExp { ivar = A.Var {v=(var_in ++ (show i') ++ var_in ++ (show i) ++ "_" ++ (show a)),
                                                               idx=Nothing },
                                                 iexp = ppentry }
                          sumentry = A.VarExp A.Var { v=(var_in ++ (show i') ++ var_in ++ (show i) ++ "_" ++ (show a)),
                                                 idx=Nothing }
                          sumentry' = if (null sums')
                                      then sumentry
                                      else let e = head sums'
                                           in A.OpExp {left=sumentry,
                                                      oper=A.PlusOp,
                                                      right=e}
                      in (ps''', (ppentry':pairprod') , [sumentry'])
  gen_feSquare_sums' _ _ _ a b c _ _ _ _ = (a,b,c)

  gen_feSquare_init_coeff :: String -> [[Int]] -> Int -> [A.Exp]
  gen_feSquare_init_coeff _ [] _ = []
  gen_feSquare_init_coeff var_in (a:as) idx =
    let t = gen_feSquare_init_coeff var_in as (idx + 1)
        vr = var_in ++ show(idx)
        h = gen_feSquare_init_coeff' vr a
    in h ++ t

  gen_feSquare_init_coeff' :: String -> [Int] -> [A.Exp]
  gen_feSquare_init_coeff' _ [] = []
  gen_feSquare_init_coeff' va (a:as) =
    let t = gen_feSquare_init_coeff' va as
        vr = A.Var { v=(va ++ "_" ++ (show a)),
                            idx=Nothing }
        r  = A.OpExp { left=A.IntExp(a),
                               oper=A.TimesOp,
                               right=A.VarExp A.Var { v=va, idx=Nothing } }
        h  = A.InitExp { ivar = vr, iexp = r }
     in if (a == 1)
        then t
        else h : t


  gen_feSquare_init_off :: String -> [Int] -> Int -> [A.Exp]
  gen_feSquare_init_off _ [] _ = []
  gen_feSquare_init_off var_in (a:as) idx =
    let t = gen_feSquare_init_off var_in as (idx+1)
    in if (a == 0)
       then t
       else let va = var_in ++ show(idx)
                vr = A.Var { v=(va ++ "_" ++ show(a)),
                            idx=Nothing }
                r  = A.OpExp { left=A.IntExp(a),
                               oper=A.TimesOp,
                               right=A.VarExp A.Var { v=va, idx=Nothing } }
                h  = A.InitExp { ivar = vr, iexp = r }
            in h : t



-----------------------------------
--                               --
--         gen_feSquare2         --
--                               --
-----------------------------------

-- TODO: verify things fit, carry logic
-- basically the same as gen_feSquare
  gen_feSquare2 :: P.Params -> A.Dec
  gen_feSquare2 p =
    let var_in       = "f"
        var_out      = "h"
        param1       = A.Param { pvar=var_out, ptyp=Nothing }
        param2       = A.Param { pvar=var_in, ptyp=Just "*fieldElement" }
        v1           = reverse (gen_feMul_load var_in (len p))
        os           = reverse (scanr1 (+) (rep p))
        os'          = [n - (os !! 0) | n <- os]
        ra           = reverse (gen_feMul_repeq (base p) os' os' (os' ++ os'))
        ca           = gen_feSquare_coeff ra (len p) (offset p) 0
        moa          = gen_feSquare_min_off (gen_feSquare_reindex ca) 0 (len p)
        rca          = gen_feSquare_red_coeff ca moa
        pca          = gen_feSquare_precomp_coeff (gen_feSquare_reindex rca)
        init         = gen_feSquare_init (len p)
        (da, v4, v5) = gen_feSquare_sums ca moa pca init [] [] var_in (len p) 0
        v4'          = O.schedule (opt p) v4
        v2           = gen_feSquare_init_coeff var_in da 0
        v3           = gen_feSquare_init_off var_in moa 0
        v6           = A.VarDecExp { vd=[A.Var {v="carry", idx=Nothing}], 
                                     typ=("[" ++ show (len p) ++ "]int64") }
        v7           = reverse (gen_feSquare_double var_out (len p))
        v8           = reverse (gen_feMul_save var_out (len p))
        v            = O.prune (opt p) [v1, v2, v3, v4', (reverse v5), [v6], v7, v8] [param1, param2]
    in A.FunctionDec {fd = "feSquare2",
                      params=[param1, param2],
                      result=Nothing,
                      body=A.SeqExp v}

  gen_feSquare_double :: String -> Int -> [A.Exp]
  gen_feSquare_double v_out 0 = []
  gen_feSquare_double v_out idx =
    let var_out = A.Var { v=(v_out ++ (show (idx-1))),
                      idx=Nothing }
        h       = AssignExp { var=var_out,
                              aexp=A.VarExp var_out, 
                              aoper=Just A.PlusOp }
        t       = gen_feSquare_double v_out (idx-1)
    in h : t

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

  -- gets the odd terms in the first len elements of list [a]
  accumOdds :: Int -> [a] -> [a]
  accumOdds 0 xs = []
  accumOdds n (x:xs) = x : (accumEvens (n-1) xs)

  -- gets the even terms in the first len elements of list [a]
  accumEvens :: Int -> [a] -> [a]
  accumEvens 0 xs = []
  accumEvens n (x:xs) = accumOdds (n-1) xs

  printListOfLists :: [[Int]] -> [A.Exp]
  printListOfLists [] = []
  printListOfLists (a:as) =
    let x = map (A.IntExp) a
        y = x ++ [A.NewLineExp]
     in y ++ (printListOfLists as)

  appendnth :: [[Int]] -> Int -> Int -> [[Int]]
  appendnth [] _ _ = []
  appendnth (l:ls) idx entry =
    if (idx == 0)
    then ( (nub (entry : l)) : ls)
    else l : (appendnth ls (idx-1) entry)

