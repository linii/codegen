module Main where

  import Gen as G
  import Export as P
  import Params as Pm
  import Data.List

  main :: IO ()
  main = putStr (P.printAst (G.genAst (Pm.gen25519)))
