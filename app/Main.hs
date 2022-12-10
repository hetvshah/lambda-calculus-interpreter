module Main where

import Data.List as List
import LCEvaluator qualified as E
import LCParser qualified as P
import LCSyntax qualified as S
import System.IO

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  putStrLn "\nWelcome to Lambda Calculus Interpreter!"
  putStrLn "':l filename reductType' to interpret from a file."
  putStrLn "':t reductType' to specify type of reduction (beta, eta)."
  putStrLn "or type in your favorite LC expression :D\n"
  mainLoop S.Beta E.initialStore
  where
    mainLoop :: S.ReductionType -> E.Store -> IO ()
    mainLoop rt store = do
      putStr "--> "
      str <- getLine
      case List.uncons (words str) of
        Just (":l", [fn, reductType]) -> do
          undefined
        Just (":t", [reductType]) -> mainLoop (S.typeToEnum reductType) store
        Just _ -> case P.parseLCStat str of
          Right stat -> case stat of
            S.Assign var exp -> do
              let new_store = E.evalAddDef var exp store
              mainLoop rt new_store -- State.evalState (E.addDef var exp)
            S.Expression exp -> do
              let v = E.evalBetaReduce exp store
              putStrLn (S.pretty v)
              mainLoop rt store
          Left _s -> do
            putStrLn _s
            mainLoop rt store
        Nothing -> mainLoop rt store