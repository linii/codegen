-- param module
-- builds an optimal set of params
-- given the input of a param
-- CURRENTLY HARDCODED TO Curve25519

module Param where

  data Params = Params {base :: Int, 
                        offset :: Int, 
                        sign :: Sign, 
                        rep :: [Int],
                        len :: Int}

  data Sign = Positive | Negative

  -- currently harcoded for ed25519
  -- the prime is encoded as 2^base + sign * offeset
  -- rep is a least-to-most significant breakdown of how to represent
  -- elements in the field (this might be the hardest part)
  -- length is just the number of elements in rep

  -- we assume that the sign is always negative for now

  genParams :: String -> Params
  genParams s = Params {base=255,
                        offset=19,
                        sign=Negative,
                        rep=[26,25,26,25,26,25,26,25,26,25],
                        len=10}