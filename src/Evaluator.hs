module Evaluator where

import Data.Set (Set)
import Data.Set qualified as Set
import Lib -- (Exp (..), Var)

-- Gets all variables from an expression
getVars :: Exp -> Set Var
getVars exp = case exp of
  Var v -> Set.singleton v
  Fun v e -> Set.insert v (getVars e)
  App e1 e2 -> Set.union (getVars e1) (getVars e2)
  Int _ -> Set.empty
  Bool _ -> Set.empty
  BopE _ e1 e2 -> Set.union (getVars e1) (getVars e2)
  UopE _ e -> getVars e

-- Gets the free variables from an expression
getFreeVars :: Exp -> Set Var
getFreeVars exp = case exp of
  Var v -> Set.singleton v
  Fun v e -> Set.filter (/= v) (getFreeVars e)
  App e1 e2 -> Set.union (getFreeVars e1) (getFreeVars e2)
  Int _ -> Set.empty
  Bool _ -> Set.empty
  BopE _ e1 e2 -> Set.union (getFreeVars e1) (getFreeVars e2)
  UopE _ e -> getFreeVars e

-- Gets all the function arguments from an expression
getArgs :: Exp -> Set Var
getArgs exp = case exp of
  Var v -> Set.empty
  Fun v e -> Set.singleton v
  App e1 e2 -> Set.union (getArgs e1) (getArgs e2)
  Int _ -> Set.empty
  Bool _ -> Set.empty
  BopE _ e1 e2 -> Set.union (getArgs e1) (getArgs e2)
  UopE _ e -> getArgs e

-- Changes name of bound variables via capture-avoiding substitution
alphaConverter :: Exp -> Exp
alphaConverter exp = undefined

-- Substitute every var v with exp vExp in exp
substitute :: Var -> Exp -> Exp -> Exp
substitute v vExp exp = undefined

-- Evaluates/simplies the expression through beta reduction
-- Substitutes and evaluates
betaReducer :: Exp -> Exp
betaReducer exp = undefined

-- Reduces expressions (optimization), used before betaReducer
-- \x -> f x ----> f
etaConverter :: Exp -> Exp
etaConverter exp = undefined