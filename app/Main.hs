module Main where

import Data.List as List
import LCEvaluator qualified as E
import LCParser qualified as P
import LCSyntax qualified as S
import System.IO

data ReductionType = Beta | Eta

typeToEnum :: String -> ReductionType
typeToEnum str = case str of
  "beta" -> Beta
  "eta" -> Eta
  _ -> error "shouldn't come here"

main :: IO ()
main = do
  putStrLn "Welcome to Lambda Calculus Interpreter!"
  putStrLn "':l filename reductType' to interpret from a file."
  putStrLn "':t reductType' to specify type of reduction (beta, eta)."
  putStrLn "or type in your favorite LC expression :D"
  mainLoop Beta
  where
    mainLoop rt = do
      hSetBuffering stdin LineBuffering
      putStr "--> "
      str <- getLine
      case List.uncons (words str) of
        Just (":l", [fn, reductType]) -> do
          undefined
        Just (":t", [reductType]) -> mainLoop (typeToEnum reductType)
        Just _ -> case P.parseLCExp str of
          Right exp -> do
            let v = E.evalBetaReduce exp E.initialStore
            putStrLn (S.pretty v)
            mainLoop rt
          Left _s -> do
            putStrLn _s
            mainLoop rt
        Nothing -> mainLoop rt

-- 0 : beta reduce
-- 1 : eta reduce