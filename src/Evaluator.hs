module Evaluator where

import Data.Set (Set)
import Data.Set qualified as Set
import Lib -- (Exp (..), Var)

-- Gets the free variables from an expression
getFreeVarsFromExp :: Exp -> Set Var
getFreeVarsFromExp exp = case exp of
  Var v -> Set.singleton v
  Fun v e -> Set.filter (/= v) (getFreeVarsFromExp e)
  App e1 e2 -> Set.union (getFreeVarsFromExp e1) (getFreeVarsFromExp e2)
  Int _ -> Set.empty
  Bool _ -> Set.empty
  BopE _ e1 e2 -> Set.union (getFreeVarsFromExp e1) (getFreeVarsFromExp e2)
  UopE _ e -> getFreeVarsFromExp e

-- Changes name of bound variables via capture-avoiding substitution
alphaConverter :: Set Var -> Exp -> Exp
alphaConverter freeVars exp = undefined

substitute :: Var -> Exp -> Exp -> Exp
substitute e v = undefined

evaluate :: Exp -> Exp
evaluate = undefined

-- Evaluates/simplies the expression through beta reduction
-- Substitutes and evaluates
betaReducer :: Exp -> Exp
betaReducer exp = undefined

-- Reduces expressions (optimization), used before betaReducer
etaConverter :: Exp -> Exp
etaConverter exp = undefined