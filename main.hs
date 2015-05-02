-- main module for the Crypto CodeGen Haskell utility
-- takes a prime as input and generates Go code
-- for fast scalar multiplcation routines

module Main where

  import Gen as G
  import Print as P

  fegen :: String -> IO ()
  fegen a = putStr (P.printAbsyn (G.genAbsyn a))