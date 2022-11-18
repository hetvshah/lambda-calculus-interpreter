module Evaluator where

import Lib (Exp (..), Var)

-- Gets the free variables from an expression
getFreeVars :: Exp -> [Var]
getFreeVars exp = case exp of
  Var v -> [v]
  Fun v e -> [v' | v' <- getFreeVars e, v' /= v]
  App e1 e2 -> getFreeVars e1 ++ getFreeVars e2

-- Changes name of bound variables via capture-avoiding substitution
alphaConverter :: [Var] -> Exp -> Exp
alphaConverter freeVars exp = undefined

-- Evaluates/simplies the expression through beta reduction
betaReducer :: Exp -> Exp
betaReducer exp = undefined