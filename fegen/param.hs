-- param module
-- builds an optimal set of params
-- given the input of a param

module Param where

  import Data.List

  data Params = Params {base :: Int, 
                        offset :: Int, 
                        sign :: Sign, 
                        rep :: [Int],
                        len :: Int,
                        opt :: Bool}

  data Sign = Positive | Negative

-- the prime is encoded as 2^base + sign * offeset
-- rep is a least-to-most significant breakdown of how to represent
-- elements in the field (this might be the hardest part)
-- length is just the number of elements in rep
--
-- we assume that the sign is always negative for now

  genParams :: String -> Params
  genParams s = 
    let o   = if (isInfixOf "--unopt" s || isInfixOf "-u" s)
              then False
              else True
    in if (isInfixOf "25519" s)
       then gen25519 o
       else (if isInfixOf "7615" s
             then gen7615 o
             else error "Sorry, do not recognize that input.") 

  gen25519 :: Bool -> Params
  gen25519 o =
    Params {  base=255,
              offset=19,
              sign=Negative,
              rep=[26,25,26,25,26,25,26,25,26,25],
              len=10,
              opt=o }

  gen7615 :: Bool -> Params
  gen7615 o =
    Params {  base=76,
              offset=15,
              sign=Negative,
              rep=[26,25,25],
              len=3,
              opt=o }
