module Evaluator where

-- (Exp (..), Var)
import Control.Monad.State (State)
import Control.Monad.State qualified as S
import Data.Set (Set)
import Data.Set qualified as Set
import Lib

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

type Store = Int

incr :: State Store Int
incr = do
  x <- S.get
  S.put (x + 1)
  return $ x + 1

getFreshVar :: Var -> Set Var -> State Store Var
getFreshVar v freeVars = do
  nextCount <- incr
  let newVar = v ++ show nextCount
  if Set.member newVar freeVars then getFreshVar v freeVars else return newVar

-- let subst (e1 : exp) (x : var) (e2 : exp) : exp =
--   let rec sub (vars : VarSet.t) (e1 : exp) (x : var) (e2 : exp) : exp =
-- \| Fun (arg, body) ->
--         if arg = x then Fun (arg, body)
--         else if VarSet.mem arg (free_vars e1) then
--           let arg' = fresh_var_for vars arg in
--           let vars' = VarSet.add arg (VarSet.add arg' vars) in
--           let body' = sub vars' (Var arg') arg body in
--           Fun(arg', sub vars' e1 x body')
--         else
--           let vars' = VarSet.add arg vars in
--           Fun(arg, sub vars' e1 x body)

-- Capture-avoiding-substitute every var v with exp vExp in exp
substitute :: Var -> Exp -> Exp -> State Store Exp
substitute v vExp exp = sub vars v vExp exp
  where
    vars :: Set Var
    vars = Set.union (getFreeVars vExp) (getFreeVars exp)
    sub :: Set Var -> Var -> Exp -> Exp -> State Store Exp
    sub vars' v' vExp' exp' = case exp' of
      Var v -> if v == v' then return vExp' else return $ Var v
      Fun v e ->
        if v == v'
          then return $ Fun v e
          else
            if Set.member v (getFreeVars vExp)
              then do
                -- alpha conversion
                arg' <- getFreshVar v (getFreeVars vExp)
                let vars' = Set.insert v (Set.insert arg' vars)
                body' <- sub vars' arg' (Var v) e
                body'' <- sub vars' v' vExp body'
                return $ Fun arg' body''
              else Fun v <$> sub (Set.insert v vars') v' vExp' e
      App e1 e2 -> App <$> sub vars' v' vExp' e1 <*> sub vars' v' vExp' e2
      Int _ -> return exp'
      Bool _ -> return exp'
      BopE o e1 e2 -> BopE o <$> sub vars' v' vExp' e1 <*> sub vars' v' vExp' e2
      UopE o e -> UopE o <$> sub vars' v' vExp' e

evalSubstitute :: Var -> Exp -> Exp -> Store -> Exp
evalSubstitute v vExp exp = S.evalState (substitute v vExp exp)

-- Weak head normal form - simplifies lambda terms lazily until it gets stuck on a variable application
whnf :: Exp -> State Int Exp
whnf exp = case exp of
  App e1 e2 -> do
    rec_call <- whnf e1
    case rec_call of
      Var v -> return $ App (Var v) e2
      Fun v e -> do
        m <- substitute v e2 e
        whnf m
      _ -> return $ App e1 e2
  _ -> return exp

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
betaReducer :: Exp -> State Store Exp
betaReducer exp = case exp of
  Fun v e -> Fun v <$> betaReducer e
  App e1 e2 -> do
    whnf_e1 <- whnf e1
    case whnf_e1 of
      Fun v body -> do
        sub_val <- substitute v e2 body
        betaReducer sub_val
      e -> App <$> betaReducer e <*> betaReducer e2
  BopE o e1 e2 -> return $ evalBop o e1 e2
  UopE o e -> return $ evalUop o e
  _ -> return exp

evalBetaReducer :: Exp -> Store -> Exp
evalBetaReducer exp = S.evalState (betaReducer exp)

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
