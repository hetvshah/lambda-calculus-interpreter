module LCEvaluator where

import Control.Monad (join, unless, when, (>=>))
import Control.Monad.State (State)
import Control.Monad.State qualified as S
import Data.List qualified as List
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import LCParser
import LCSyntax

type Store = (Int, Def)

type Def = Map Var Exp

-- Gets all variables from an expression
getVars :: Exp -> Set Var
getVars exp = case exp of
  Var v -> Set.singleton v
  Fun v e -> Set.insert v (getVars e)
  App e1 e2 -> Set.union (getVars e1) (getVars e2)
  IntE _ -> Set.empty
  BoolE _ -> Set.empty
  BopE _ e1 e2 -> Set.union (getVars e1) (getVars e2)
  UopE _ e -> getVars e

-- Gets the free variables from an expression
getFreeVars :: Exp -> Set Var
getFreeVars exp = case exp of
  Var v -> Set.singleton v
  Fun v e -> Set.filter (/= v) (getFreeVars e)
  App e1 e2 -> Set.union (getFreeVars e1) (getFreeVars e2)
  IntE _ -> Set.empty
  BoolE _ -> Set.empty
  BopE _ e1 e2 -> Set.union (getFreeVars e1) (getFreeVars e2)
  UopE _ e -> getFreeVars e

-- Gets all the function arguments from an expression
getArgs :: Exp -> Set Var
getArgs exp = case exp of
  Var v -> Set.empty
  Fun v e -> Set.singleton v
  App e1 e2 -> Set.union (getArgs e1) (getArgs e2)
  IntE _ -> Set.empty
  BoolE _ -> Set.empty
  BopE _ e1 e2 -> Set.union (getArgs e1) (getArgs e2)
  UopE _ e -> getArgs e

-- Renames var in exp with a uniquely generated name
alphaConverter :: Var -> Exp -> Exp
alphaConverter exp = undefined

incr :: State Store Int
incr = do
  (x, d) <- S.get
  S.put (x + 1, d)
  return $ x + 1

getFreshVar :: Var -> Set Var -> State Store Var
getFreshVar v freeVars = do
  nextCount <- incr
  let newVar = v ++ show nextCount
  if Set.member newVar freeVars then getFreshVar v freeVars else return newVar

-- Capture-avoiding-substitute every var v with exp vExp in exp
substitute :: Var -> Exp -> Exp -> State Store Exp
substitute v'' vExp exp = sub vars v'' vExp exp
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
            if Set.member v (getFreeVars vExp')
              then do
                -- alpha conversion
                v1 <- getFreshVar v vars
                let vars' = Set.insert v (Set.insert v1 vars)
                body' <- sub vars' v (Var v1) e
                body'' <- sub vars' v' vExp' body'
                return $ Fun v1 body''
              else Fun v <$> sub (Set.insert v vars') v' vExp' e
      App e1 e2 -> App <$> sub vars' v' vExp' e1 <*> sub vars' v' vExp' e2
      IntE _ -> return exp'
      BoolE _ -> return exp'
      BopE o e1 e2 -> BopE o <$> sub vars' v' vExp' e1 <*> sub vars' v' vExp' e2
      UopE o e -> UopE o <$> sub vars' v' vExp' e

evalSubstitute :: Var -> Exp -> Exp -> Store -> Exp
evalSubstitute v vExp exp = S.evalState (substitute v vExp exp)

-- Weak head normal form - simplifies lambda terms lazily until it gets stuck on a variable application
whnf :: Exp -> State Store Exp
whnf exp = case exp of
  Var v -> do
    (_, def) <- S.get
    case Map.lookup v def of
      Nothing -> return exp
      Just e -> return e
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
evalBop Plus (IntE x) (IntE y) = IntE $ x + y
evalBop Minus (IntE x) (IntE y) = IntE $ x - y
evalBop Times (IntE x) (IntE y) = IntE $ x * y
evalBop Divide (IntE x) (IntE y) = IntE $ x `div` y
evalBop Modulo (IntE x) (IntE y) = IntE $ x `mod` y
evalBop Eq x y = BoolE $ x == y
evalBop Gt (IntE x) (IntE y) = BoolE $ x > y
evalBop Ge (IntE x) (IntE y) = BoolE $ x >= y
evalBop Lt (IntE x) (IntE y) = evalBop Gt (IntE y) (IntE x)
evalBop Le (IntE x) (IntE y) = evalBop Ge (IntE y) (IntE x)
evalBop o e1 e2 = BopE o e1 e2

-- Evaluate a unary operation expression
evalUop :: Uop -> Exp -> Exp
evalUop Neg (IntE i) = IntE (-i)
evalUop Neg (BoolE b) = error "not well typed"
evalUop Not (IntE i) = if i > 0 then IntE 0 else IntE 1
evalUop Not (BoolE b) = BoolE (not b)
evalUop o e = UopE o e

-- Evaluates/simplies the expression through beta reduction
-- Substitutes and evaluates
betaReduce :: Exp -> State Store Exp
betaReduce exp = case exp of
  Fun v e -> do
    (_, def) <- S.get
    case Map.lookup v def of
      Nothing -> Fun v <$> betaReduce e
      Just _ -> do
        new_var <- getFreshVar v (getFreeVars e)
        new_e <- substitute v (Var new_var) e
        Fun new_var <$> betaReduce new_e
  App e1 e2 -> do
    whnf_e1 <- whnf e1
    case whnf_e1 of
      Fun v body -> do
        sub_val <- substitute v e2 body
        betaReduce sub_val
      e -> App <$> betaReduce e <*> betaReduce e2
  BopE o e1 e2 -> evalBop o <$> betaReduce e1 <*> betaReduce e2
  UopE o e -> evalUop o <$> betaReduce e
  Var v -> do
    (_, def) <- S.get
    case Map.lookup v def of
      Nothing -> return exp
      Just e -> return e
  _ -> return exp

evalBetaReduce :: Exp -> Store -> Exp
evalBetaReduce exp = S.evalState (betaReduce exp)

-- Reduces expressions
-- \x -> f x ----> f
etaReduce :: Exp -> Exp
etaReduce exp = case exp of
  Fun v (App (Fun v' body) (Var x)) ->
    if v == x
      then if Set.member v (getFreeVars body) then exp else Fun v' body
      else exp
  _ -> exp

initialStore :: Store
initialStore = (0, Map.empty)

-- Return true if var is in the map of declarations, false ow
inDef :: Var -> State Store Bool
inDef v = do
  (_, def) <- S.get
  return $ Map.member v def

addDef :: Var -> Exp -> State Store ()
addDef var exp = do
  let frees = getFreeVars exp
  freesInDef <- List.foldr (\v acc -> inDef v >>= \b -> acc >>= \a -> return (b && a)) (return True) frees
  if freesInDef
    then do
      (amt, currDefs) <- S.get
      exp' <- betaReduce exp
      S.put (amt, Map.insert var exp' currDefs)
    else error "free var exists in exp but is not defined"

evalAddDef :: Var -> Exp -> Store -> Store
evalAddDef v exp = S.execState (addDef v exp)

evalS :: Statement -> State Store ()
evalS (Assign v exp) = undefined
evalS (Expression exp) = undefined
