module Main where

import LCEvaluator qualified as E
import LCParser qualified as P
import LCSyntax qualified as S
import System.IO

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  putStr "--> "
  str <- getLine
  case P.parseLCExp str of
    Right exp -> do
      let v = E.evalBetaReduce exp E.initialStore
      putStrLn (S.pretty v)
      main
    Left _s -> do
      putStrLn _s
      main
