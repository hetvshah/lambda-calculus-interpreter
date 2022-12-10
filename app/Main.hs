module Main where

import Control.Monad (when)
import Data.List as List
import LCEvaluator qualified as E
import LCParser qualified as P
import LCSyntax qualified as S
-- import System.IO
import System.IO qualified as IO
import System.IO.Error qualified as IO

main :: IO ()
main = do
  IO.hSetBuffering IO.stdin IO.LineBuffering
  putStrLn "\nWelcome to ƛ-Calculus Interpreter!"
  putStrLn "':l filename reductType' to interpret from a file."
  putStrLn "':t reductType' to specify type of reduction (beta, eta, beta-eta)."
  putStrLn "or type in your favorite LC expression 🙃\n"
  mainLoop S.Beta E.initialStore IO.stdin
  where
    mainLoop :: S.ReductionType -> E.Store -> IO.Handle -> IO ()
    mainLoop rt store file = do
      when (file == IO.stdin) $ putStr "--> "
      str <- IO.hGetLine file
      case List.uncons (words str) of
        Just (":l", [fn, reductType]) ->
          IO.catchIOError
            ( do
                x <- IO.openFile fn IO.ReadMode
                mainLoop (S.typeToEnum reductType) E.initialStore x
            )
            ( \e -> mainLoop rt store file
            )
        Just (":t", [reductType]) -> mainLoop (S.typeToEnum reductType) store file
        Just _ -> case P.parseLCStat str of
          Right stat -> case stat of
            S.Assign var exp -> do
              let new_store = E.evalAddDef var exp store -- need to case on rt
              mainLoop rt new_store file
            S.Expression exp -> do
              let v = E.evalReduce rt exp store
              -- E.evalBetaReduce exp store -- need to case on rt
              putStrLn (S.pretty v)
              mainLoop rt store file
          Left _s -> do
            putStrLn _s
            mainLoop rt store file
        Nothing -> mainLoop rt store file
