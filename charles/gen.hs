-- codegen module

module Gen where

  import Absyn as A
  import Param as P

  genAbsyn :: String -> A.Prog
  genAbsyn s = 
    let p = P.genParams s
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
                gen_load4 p,
                gen_feFromBytes p ]

-----------------------------------
--                               --
--          easy stuff           --
--                               --
-----------------------------------

  gen_Package :: Params -> A.Dec
  gen_Package p = A.PackageDec "scalarmult"

  gen_Imports :: Params -> A.Dec
  gen_Imports p = A.ImportDec "fml"

  gen_Type :: Params -> A.Dec
  gen_Type p = A.TypeDec {td="fieldElement", ty="int32", size=(len p)}

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
  gen_feSub p =
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

-- a general way to load
-- super unclean code by what can you do eh
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

-----------------------------------
--                               --
--        gen_feFromBytes        --
--                               --
-----------------------------------

  gen_feFromBytes :: Params -> A.Dec
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
              a = A.TypeCastExp { tcvar=A.Var{v=("h"++(show cur')),
                                              idx=Nothing },
                                  tctyp="int32" }
              e = A.AssignExp { var=A.Var{v="dst", idx=Just (show cur')},
                                aexp=a,
                                aoper=Nothing}
          in e : (gen_feFromBytes_save' len (cur-1))



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

