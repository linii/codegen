 module Params where

  data Params = Params { base :: Int, offset :: Int, sign :: Sign, rep :: [Int], len :: Int }
  data Sign = Negative | Positive

  gen25519 :: Params
  gen25519 =
    Params {  base=255,
              offset=19,
              sign=Negative,
              rep=[26,25,26,25,26,25,26,25,26,25],
              len=10}
