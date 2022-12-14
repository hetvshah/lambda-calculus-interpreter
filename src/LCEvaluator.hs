module LCEvaluator where

import Control.Monad (join, unless, when, (>=>))
import Control.Monad.State (State)
import Control.Monad.State qualified as S
import Data.Bifunctor (second)
import Data.List qualified as List
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import LCParser
import LCSyntax

type Store = (Int, Def)

type Def = Map Var (Exp, Bool)

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
  Fun v e -> Set.union (Set.singleton v) (getArgs e)
  App e1 e2 -> Set.union (getArgs e1) (getArgs e2)
  IntE _ -> Set.empty
  BoolE _ -> Set.empty
  BopE _ e1 e2 -> Set.union (getArgs e1) (getArgs e2)
  UopE _ e -> getArgs e

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
      Just (e, reduced) -> return e
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
evalBop Divide (IntE x) (IntE 0) = BopE Divide (IntE x) (IntE 0)
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
evalUop Not (IntE i) = if i > 0 then IntE 0 else IntE 1
evalUop Not (BoolE b) = BoolE (not b)
evalUop o e = UopE o e

reduceVar :: CallByType -> Var -> (CallByType -> Exp -> State Store Exp) -> State Store Exp
reduceVar ct v reduceFun = do 
  (count, def) <- S.get 
  case Map.lookup v def of
    Nothing -> return (Var v)
    Just (e, reduced) -> do 
      case ct of 
        Name -> reduceFun ct e
        Need -> if reduced then return e else do 
          reducedExp <- reduceFun ct e
          S.put (count, Map.insert v (reducedExp, True) def)
          return reducedExp

-- Evaluates/simplies the expression through beta reduction
-- Substitutes and evaluates
betaReduce :: CallByType -> Exp -> State Store Exp
betaReduce ct exp = case exp of
  Fun v e -> do
    (_, def) <- S.get
    case Map.lookup v def of
      Nothing -> Fun v <$> betaReduce ct e
      Just _ -> do
        new_var <- getFreshVar v (getFreeVars e)
        new_e <- substitute v (Var new_var) e
        Fun new_var <$> betaReduce ct new_e
  App e1 e2 -> do
    whnf_e1 <- whnf e1
    case whnf_e1 of
      Fun v body -> do
        sub_val <- substitute v e2 body
        betaReduce ct sub_val
      e -> App <$> betaReduce ct e <*> betaReduce ct e2
  BopE o e1 e2 -> evalBop o <$> betaReduce ct e1 <*> betaReduce ct e2
  UopE o e -> evalUop o <$> betaReduce ct e
  Var v -> reduceVar ct v betaReduce
  _ -> return exp

evalBetaReduce :: CallByType -> Exp -> Store -> Exp
evalBetaReduce ct exp = S.evalState (betaReduce ct exp)

-- Reduces expressions
-- \x -> f x ----> f
etaReduce :: CallByType -> Exp -> State Store Exp
etaReduce ct exp = case exp of
  Fun v (App (Fun v' body) (Var x)) ->
    if v == x
      then if Set.member v (getFreeVars body) then return exp else return $ Fun v' body
      else return exp
  Var v -> reduceVar ct v etaReduce
  _ -> return exp

evalEtaReduce :: CallByType -> Exp -> Store -> Exp
evalEtaReduce ct exp = S.evalState (etaReduce ct exp)

initialStore :: Store
initialStore = (0, Map.empty)

-- Return true if var is in the map of declarations, false ow
inDef :: Var -> State Store Bool
inDef v = do
  (_, def) <- S.get
  return $ Map.member v def

substituteAll :: Set Var -> Def -> Exp -> State Store Exp
substituteAll frees currDefs exp = Set.foldr comb (return exp) frees
  where
    comb :: Var -> State Store Exp -> State Store Exp
    comb free acc = case Map.lookup free currDefs of
      Nothing -> acc
      Just (e, reduced) -> substitute free e =<< acc

-- Call by name
addDef :: Var -> Exp -> State Store ()
addDef var exp = do
  let frees = getFreeVars exp
  freesInDef <- List.foldr (\v acc -> inDef v >>= \b -> acc >>= \a -> return (b && a)) (return True) frees
  if freesInDef
    then do
      (amt, currDefs) <- S.get
      exp' <- substituteAll frees currDefs exp
      S.put (amt, Map.insert var (exp', False) currDefs)
    else error "free var exists in exp but is not defined"

evalAddDef :: Var -> Exp -> Store -> Store
evalAddDef v exp = S.execState (addDef v exp)

-- -- Call by need --> evaluate as needed
-- addDef' :: Var -> Exp -> State Store ()
-- addDef' var exp = do
--   let frees = getFreeVars exp
--   freesInDef <- List.foldr (\v acc -> inDef v >>= \b -> acc >>= \a -> return (b && a)) (return True) frees
--   if freesInDef
--     then do
--       (amt, currDefs) <- S.get
--       exp' <- substituteAll frees currDefs exp
--       S.put (amt, Map.insert var exp' currDefs)
--     else error "free var exists in exp but is not defined"

-- evalAddDef' :: Var -> Exp -> Store -> Store
-- evalAddDef' v exp = S.execState (addDef v exp)

reduce :: ReductionType -> CallByType -> Exp -> State Store Exp
reduce Beta ct exp = betaReduce ct exp
reduce Eta ct exp = etaReduce ct exp
reduce BetaEta ct exp = etaReduce ct =<< betaReduce ct exp

evalReduce :: ReductionType -> CallByType -> Exp -> Store -> Exp
evalReduce rt ct exp = S.evalState (reduce rt ct exp)