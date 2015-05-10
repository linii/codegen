-- opt module
-- a couple post optimizations that make the Gen module easier
-- see comments before function for further assumptions on structure

module Opt where

  import Absyn as A
  import Print as P
  import Data.List
  import Data.Function

-----------------------------------
--                               --
--            pruning            --
--                               --
-----------------------------------

-- sometimes an optimization may end up precomputing an unneeded value
-- prune removes computations that aren't used
-- input is basic blocks
  prune :: Bool -> [[A.Exp]] -> [A.Param] -> [A.Exp]
  prune o a b =
    if o
    then
      let p         = map pvar b
          a'        = linearize (concat a)
          (e, u, t) = prune' a' p
          (_, l)    = partition (`elem` p) u
      in (if (null l)
          then (if t
                then prune o [e] b
                else e)
          else (let c = A.CommentExp ("Opt.prune: one or more variables in list [" 
                        ++ (intercalate ", " l)
                        ++ "] might not have been defined")
                in c : e ))
    else concat a

-- takes a list of expressions and uses seen thus far
-- and returns (list of expressions, uses)
-- where you check to see if the current def had a use after it
-- if it doesn't, then delete the current expression
-- assumes no side effects
  prune' :: [A.Exp] -> [String] -> ([A.Exp], [String], Bool)
  prune' [] u = ([], u, False)
  prune' (a:as) u =
    let (a1, u1, t)  = prune' as u
        (u2, d)      = exp_to_use_def a
        (_, p2)      = partition (`elem` d) u1
        d'           = map (takeWhile (/= '[')) d
        (p1, _)      = partition (`elem` d') u1
        (a2, t'')    = if (null p1)
                       -- then (a1, True)
                       then  (a1, True)
                       else (a:a1, False)
    in  case a of 
          A.CommentExp _ -> (a:a1, u1, t)
          A.NewLineExp -> (a:a1, u1, t)
          A.RangeExp {rvar=f, rangevar=r, rloop=b} ->
            let p             = v f
                b'            = linearize [b]
                (b'', u, t')  = prune' b' [p]
                (e, t'')      = if (null b'')
                                then (a1, True)
                                else ((A.RangeExp {rvar=f, rangevar=r, rloop=A.SeqExp b''}):a1, False)
            in (e, u ++ u1, t'' || t' || t)
          A.ForExp {fvar=f, cond=c, inc=n, finit=i, floop=b} ->
            let p             = v f
                b'            = linearize [b]
                (b'', u, t')  = prune' b' [p]
                (e, t'')      = if (null b'')
                                then (a1, True)
                                else ((A.ForExp {fvar=f, cond=c, inc=n, finit=i, floop=A.SeqExp b''}):a1, False)
            in (e, u ++ u1, t'' || t' || t)
          _ -> (a2, p2 ++ u2, t || t'')

-- linearizes a bunch of expressions
-- i.e. unpacks seqExps
  linearize :: [A.Exp] -> [A.Exp]
  linearize [] = []
  linearize (a:as) =
    let as' = linearize as
        a'  = case a of
                A.SeqExp s -> linearize s
                _ -> [a]
    in a' ++ as'


  exp_to_use_def :: A.Exp -> ([String], [String])
  exp_to_use_def e =
    case e of
      A.VarExp x -> ([v x], [])
      A.MinusExp x -> ([v x], [])
      A.AppExp {func=f, args=a} -> ((map v a), [])
      A.OpExp {left=l, oper=o, right=r} -> 
        let (u1, d1) = exp_to_use_def l
            (u2, d2) = exp_to_use_def r
        in (u1 ++ u2, d1 ++ d2)
      A.SeqExp s ->
        case s of 
          []    -> ([],[])
          x:xs  -> 
            let (u1, d1) = exp_to_use_def x
                (u2, d2) = exp_to_use_def (A.SeqExp xs)
            in (u1 ++ u2, d1 ++ d2)
      A.AssignExp {var=va, aexp=e, aoper=o} ->
        let (u, d) = exp_to_use_def e
        in (u, d ++ [P.printVar va])
      A.InitExp {ivar=va, iexp=e} ->
        let (u, d) = exp_to_use_def e
        in (u, d ++ [P.printVar va])
      A.TypeCastExp {tcexp=e, tctyp=t} -> exp_to_use_def e
      A.VarDecExp {vd=vs, typ=t} -> ([], map v vs)
      A.ParenExp a -> exp_to_use_def a
      A.ReturnExp a -> exp_to_use_def a
      _ -> ([],[])

-----------------------------------
--                               --
--          scheduling           --
--                               --
-----------------------------------

-- takes a list of assigns from binary operations
-- the point is that we want to group the maximum number of uses
-- of the same variable together as possible to minimize register traffic
-- i.e. if we have
--   a = b * c
--   d = e * f
--   g = h * b
-- then we actually want to reorder into
--   a = b * c
--   g = b * h
--   d = e * f
-- that way once b has been loaded once, it doesn't have to be
-- loaded again for the second instruction
--
-- note that we probably have enough registers here so that it doesn't matter,
-- but when the number of instructions is large, this actually does make
-- a difference
--
-- assumes SSA and block
  schedule :: Bool -> [A.Exp] -> [A.Exp]
  schedule o a = 
    if o
    then
      let p    = unwrap_src a []
          c    = make_counts p []
          a'   = reschedule p c
          a''  = map fst a'
      in a''
    else a

-- takes a list of assigns from binops like
--  a = b * c
-- and returns a list of tuples (a = b * c, (b,c))
-- where the first entry is the expression,
-- and the second is a pair of the variables used (in order)
  unwrap_src :: [A.Exp] -> [(A.Exp, (A.Var, A.Var))] -> [(A.Exp, (A.Var, A.Var))]
  unwrap_src [] p = p
  unwrap_src (a:as) p =
    let b = (case a of
              A.InitExp {ivar=_, iexp=e} -> (
                case e of 
                  A.OpExp {left=l, oper=_, right=r} -> (
                      let l' = (case l of
                                 A.TypeCastExp {tcexp=A.VarExp(x),tctyp=_} -> x
                                 A.VarExp x -> x
                                 _ -> error "Opt.unwrap_src: non vars in binop")
                          r' = (case r of
                                 A.TypeCastExp {tcexp=A.VarExp(x),tctyp=_} -> x
                                 A.VarExp x -> x
                                 _ -> error "Opt.unwrap_src: non vars in binop")
                      in (l', r'))
                  _ -> (error "Opt.unwrap_src: non binop exp"))
              _ -> (error "Opt.unwrap_src: non assign exp"))
        t = unwrap_src as p
    in (a,b):t

-- first argument is a list of expressions
-- second argument is a list of tuples
-- first tuple is times each variable is used, second is the variable name
  make_counts :: [(A.Exp, (A.Var, A.Var))] -> [(Int, A.Var)] -> [(Int, A.Var)]
  make_counts [] c = c
  make_counts ((_,(a1,a2)):as) c =
    let c1          = make_counts as c
        c2          = add_or_inc_var a1 c1
        c3          = if (A.veq a1 a2)
                      then c2
                      else add_or_inc_var a2 c2
    in c3

-- either adds a variable to the list (and starts it with a count of one)
-- or just increments the corresponding count for a variable
  add_or_inc_var :: A.Var -> [(Int, A.Var)] -> [(Int, A.Var)]
  add_or_inc_var a [] = ([(1, a)])
  add_or_inc_var a ((i, v):c) =
    if (A.veq a v)
    then (i+1, v):c
    else (i,v):add_or_inc_var a c

-- either decrements the count of a var
-- or if the count would be 0, deletes it from the list
  del_or_dec_var :: A.Var -> [(Int, A.Var)] -> [(Int, A.Var)]
  del_or_dec_var a [] = error ("Opt.del_or_dec_var: variable \"" ++ (v a) ++ "\" does not exist in list")
  del_or_dec_var a ((i,v):cs) =
    if (A.veq a v)
    then (if (i <= 1)
          then cs
          else (i-1, v):cs)
    else (i,v):(del_or_dec_var a cs)

-- takes as input a list of assign from binop expressions plus the sources
-- a list of tuples of (number of uses, source variable)
-- and returns a rescheduled list by greedily grouping together
-- the source variables that are used the most
  reschedule :: [(A.Exp, (A.Var, A.Var))] -> [(Int, A.Var)] 
                  -> [(A.Exp, (A.Var, A.Var))]
  reschedule a [] = a
  reschedule [] c = []
  reschedule a c = 
    let c1            = reverse (sortBy (compare `on` fst) c)
        (_, c2)       = splitAt 1 c1
        (_,v)         = head c1
        (h, a', c3)   = reschedule' a v c2
        t             = reschedule a' c3
    in h ++ t

-- splits a list of instructions into all the instructions that use variable v
-- and all the instructions that don't
-- also updates the list of counts as described above
  reschedule' :: [(A.Exp, (A.Var, A.Var))] -> A.Var -> [(Int, A.Var)] 
                   -> ([(A.Exp, (A.Var, A.Var))], [(A.Exp, (A.Var, A.Var))], [(Int, A.Var)])
  reschedule' [] v c = ([], [], c)
  reschedule' ((e,(v1,v2)):as) v c =
    if (A.veq v v1)
    then (let c'           = (if (A.veq v v2)
                              then c
                              else del_or_dec_var v2 c)
              (h, t, c'')  = reschedule' as v c'
              h'           = (e,(v1,v2)):h
          in (h', t, c''))
    else (if (A.veq v v2)
          then (let c'            =  (if (A.veq v v1)
                                      then c
                                      else del_or_dec_var v1 c)
                    (h, t, c'')   = reschedule' as v c'
                    e'            = rev_binop_ord e
                    h'            = (e',(v1,v2)):h
                in (h', t, c''))
          else (let (h, t, c')    = reschedule' as v c
                    t'            = (e,(v1,v2)):t
                in (h, t', c')))

-- changes a = b * c to a = c * b
  rev_binop_ord :: A.Exp -> A.Exp
  rev_binop_ord a =
    case a of
      A.InitExp {ivar=v, iexp=e} -> (
        case e of 
          A.OpExp {left=l, oper=o, right=r} -> (
                      let e'  = A.OpExp {left=r, oper=o, right=l}
                          i'  = A.InitExp {ivar=v, iexp=e'}
                      in i')
          _ -> (error "Opt.rev_binop_ord: non binop exp"))
      _ -> (error "Opt.rev_binop_ord: non assign exp")


