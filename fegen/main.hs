-- main module for the Crypto CodeGen Haskell utility
-- takes a prime as input and generates Go code
-- for fast scalar multiplcation routines

module Main where

  import Gen as G
  import Print as P
  import Param as M
  import Data.List

  main :: IO ()
  main = do putStrLn "Crypto CodeGen Haskell utility."
            putStrLn "Input \"25519\" or \"7615\" for preconfigured examples. Anything else for manual config."
            x <- getLine
            if (isInfixOf "25519" x || isInfixOf "7615" x)
            then (putStr (P.printAbsyn (G.genAbsyn (M.genSampleParams x))))
            else (do 
                      putStrLn "Enter: base of prime."
                      b <- getLine
                      putStrLn "Enter: offset of prime."
                      o <- getLine
                      putStrLn "Enter: rep of prime, least significant block first, separated by spaces."
                      r <- getLine
                      putStrLn "Enter: any other flags."
                      f <- getLine
                      fegen b o r f)


  fegen :: String -> String -> String -> String -> IO ()
  fegen b o r f = (putStr (P.printAbsyn (G.genAbsyn (M.genParams b o r f))))
