module Main where

import Control.Monad (when)
import Data.List as List
import LCEvaluator qualified as E
import LCParser qualified as P
import LCSyntax qualified as S
import System.IO qualified as IO
import System.IO.Error qualified as IO
import qualified System.Posix.Types as S

data Settings = Settings
  { store :: E.Store,
    file :: IO.Handle,
    reductionType :: S.ReductionType,
    evaluationType :: S.EvaluationType
  }

initialSettings :: Settings
initialSettings =
  Settings
    { store = E.initialStore,
      file = IO.stdin,
      reductionType = S.Beta,
      evaluationType = S.Name
    }

main :: IO ()
main = do
  IO.hSetBuffering IO.stdin IO.LineBuffering
  putStrLn "\nWelcome to ƛ-Calculus Interpreter!"
  putStrLn "':l filename reductType' to interpret from a file."
  putStrLn "':rt reductType' to specify type of reduction (beta, eta, beta-eta)."
  putStrLn "':ct evalType' to specify type of evaluation (beta, eta, beta-eta)."
  putStrLn "or type in your favorite LC expression 🙃\n"
  mainLoop initialSettings
  where
    mainLoop :: Settings -> IO ()
    mainLoop settings = do
      when (file settings == IO.stdin) $ putStr "--> "
      str <- IO.hGetLine (file settings)
      case List.uncons (words str) of
        Just (":l", fn : options) ->
          IO.catchIOError
            ( do
                x <- IO.openFile fn IO.ReadMode
                let reductType = if null options then reductionType settings else S.reductionToEnum (List.head options)
                mainLoop (settings {file = x, reductionType = reductType, store = E.initialStore})
            )
            ( \e -> mainLoop settings
            )
        Just (":rt", [reductType]) -> mainLoop (settings {reductionType = S.reductionToEnum reductType})
        Just (":et", [evalType]) -> mainLoop (settings {evaluationType = S.evaluationToEnum evalType})
        Just _ -> case P.parseLCStat str of
          Right stat -> case stat of
            S.Assign var exp -> do
              let new_store = E.evalAddDef var exp (store settings) -- need to case on rt
              mainLoop (settings {store = new_store})
            S.Expression exp -> do
              let (exp, new_store) = E.evalReduce (reductionType settings) (evaluationType settings) exp (store settings)
              putStrLn (S.pretty exp)
              mainLoop settings {store = new_store}
          Left _s -> do
            putStrLn _s
            mainLoop settings
        Nothing -> mainLoop settings
