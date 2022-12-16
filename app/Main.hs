module Main where

import Control.Monad (when)
import Data.List as List
import LCEvaluator qualified as E
import LCParser qualified as P
import LCSyntax qualified as S
import System.IO qualified as IO
import System.IO.Error qualified as IO

data Settings = Settings
  { store :: E.Store,
    file :: IO.Handle,
    reductionType :: E.ReductionType,
    evaluationType :: E.EvaluationType
  }

initialSettings :: Settings
initialSettings =
  Settings
    { store = E.initialStore,
      file = IO.stdin,
      reductionType = E.Beta,
      evaluationType = E.Name
    }

main :: IO ()
main = do
  IO.hSetBuffering IO.stdin IO.LineBuffering
  putStrLn "\nWelcome to ƛ-Calculus Interpreter!"
  putStrLn "':l filename' to interpret from a file."
  putStrLn "':rt reductionType' to specify type of reduction (beta, eta, beta-eta)."
  putStrLn "':et evaluationType' to specify type of evaluation (name, need)."
  putStrLn "or type in your favorite LC expression 🙃\n"
  mainLoop initialSettings
  where
    mainLoop :: Settings -> IO ()
    mainLoop settings = do
      when (file settings == IO.stdin) $ putStr "--> "
      str <- IO.hGetLine (file settings)
      case List.uncons (words str) of
        Just (":l", [fn]) ->
          IO.catchIOError
            ( do
                x <- IO.openFile fn IO.ReadMode
                mainLoop settings {file = x}
            )
            (\e -> mainLoop settings)
        Just (":rt", [reductType]) ->
          case E.reductionToEnum reductType of
            Nothing -> do
              putStrLn "Invalid reduction type. Must enter one of: beta, eta, beta-eta."
              mainLoop settings
            Just enum -> mainLoop (settings {reductionType = enum})
        Just (":et", [evalType]) ->
          case E.evaluationToEnum evalType of
            Nothing -> do
              putStrLn "Invalid evaluation type. Must enter one of: name, need."
              mainLoop settings
            Just enum -> mainLoop settings {evaluationType = enum}
        Just _ -> case P.parseLCStat str of
          Right stat -> case stat of
            S.Assign var exp -> do
              let new_store = E.evalAddDef var exp (store settings)
              mainLoop (settings {store = new_store})
            S.Expression exp -> do
              let (exp', new_store) = E.evalReduce (reductionType settings) (evaluationType settings) exp (store settings)
              putStrLn (S.pretty exp')
              mainLoop settings {store = new_store}
          Left _s -> do
            putStrLn _s
            mainLoop settings
        Nothing -> mainLoop settings