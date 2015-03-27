module Main where

  import Gen as G
  import Print as P

  fegen :: String -> IO ()
  fegen a = putStr (P.printAbsyn (G.genAbsyn a))