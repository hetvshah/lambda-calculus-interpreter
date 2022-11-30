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

-- Renames var in exp with a uniquely generated name
alphaConverter :: Var -> Exp -> Exp
alphaConverter exp = undefined

-- Capture-avoiding-substitute every var v with exp vExp in exp
substitute :: Var -> Exp -> Exp -> Exp
substitute v vExp exp = sub vars v vExp exp
  where
    vars :: Set Var
    vars = Set.union (getFreeVars vExp) (getFreeVars exp)
    sub :: Set Var -> Var -> Exp -> Exp -> Exp
    sub vars' v' vExp' exp' = case exp' of
      Var v -> if v == v' then vExp' else Var v
      Fun v e ->
        if v == v'
          then Fun v e
          else
            if Set.member v (getFreeVars vExp)
              then -- TODO: alpha convert
              -- let arg' = fresh_var_for vars arg in
              -- let vars' = VarSet.add arg (VarSet.add arg' vars) in
              -- let body' = sub vars' (Var arg') arg body in
              -- Fun(arg', sub vars' e1 x body')
                undefined
              else Fun v (sub (Set.insert v vars') v' vExp' e)
      App e1 e2 -> App (sub vars' v' vExp' e1) (sub vars' v' vExp' e2)
      Int _ -> exp
      Bool _ -> exp
      BopE o e1 e2 -> BopE o (sub vars' v' vExp' e1) (sub vars' v' vExp' e2)
      UopE o e -> UopE o $ sub vars' v' vExp' e

-- Weak head normal form - simplifies lambda terms lazily until it gets stuck on a variable application
whnf :: Exp -> Exp
whnf exp = case exp of
  App e1 e2 ->
    case whnf e1 of
      Var v -> App (Var v) e2
      Fun v e -> whnf $ substitute v e2 e
      _ -> App e1 e2
  _ -> exp

-- Evaluate a binary operation expression
evalBop :: Bop -> Exp -> Exp -> Exp
evalBop Plus (Int x) (Int y) = Int $ x + y
evalBop Minus (Int x) (Int y) = Int $ x - y
evalBop Times (Int x) (Int y) = Int $ x * y
evalBop Divide (Int x) (Int y) = Int $ x `div` y
evalBop Modulo (Int x) (Int y) = Int $ x `mod` y
evalBop Eq x y = Bool $ x == y
evalBop Gt (Int x) (Int y) = Bool $ x > y
evalBop Ge (Int x) (Int y) = Bool $ x >= y
evalBop Lt (Int x) (Int y) = evalBop Gt (Int y) (Int x)
evalBop Le (Int x) (Int y) = evalBop Ge (Int y) (Int x)
evalBop o e1 e2 = BopE o e1 e2

-- Evaluate a unary operation expression
evalUop :: Uop -> Exp -> Exp
evalUop Neg (Int i) = Int (-i)
evalUop Neg (Bool b) = error "not well typed"
evalUop Not (Int i) = if i > 0 then Int 0 else Int 1
evalUop Not (Bool b) = Bool (not b)
evalUop o e = UopE o e

-- Evaluates/simplies the expression through beta reduction
-- Substitutes and evaluates
betaReducer :: Exp -> Exp
betaReducer exp = case exp of
  Fun v e -> Fun v $ betaReducer e
  App e1 e2 -> case whnf e1 of
    Fun v body -> betaReducer $ substitute v e2 body
    e -> App (betaReducer e) (betaReducer e2)
  BopE o e1 e2 -> evalBop o e1 e2
  UopE o e -> evalUop o e
  _ -> exp

-- Reduces expressions
-- \x -> f x ----> f
etaConverter :: Exp -> Exp
etaConverter exp = case exp of
  Fun v (App (Fun v' body) (Var x)) ->
    if v == x
      then if Set.member v (getFreeVars body) then exp else Fun v' body
      else exp
  _ -> exp

-- Checkpoint 1:
-- y combinator for recursion
-- Rename function
-- State monad for name generation
