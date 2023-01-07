module LCEvaluator
  ( evalSubstitute,
    evalAddDef,
    evalReduce,
    evalBetaReduce,
    evalEtaReduce,
    getFreeVars,
    ReductionType (..),
    EvaluationType (..),
    reductionToEnum,
    evaluationToEnum,
    initialStore,
    Store,
  )
where

import Control.Monad.State (State)
import Control.Monad.State qualified as S
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import LCSyntax (Bop (..), Exp (..), Uop (..), Var)

-------------------------------- Type Definitions --------------------------------

data ReductionType = Beta | Eta | BetaEta

reductionToEnum :: String -> Maybe ReductionType
reductionToEnum str = case str of
  "beta" -> Just Beta
  "eta" -> Just Eta
  "beta-eta" -> Just BetaEta
  _ -> Nothing

data EvaluationType = Name | Need

evaluationToEnum :: String -> Maybe EvaluationType
evaluationToEnum str = case str of
  "name" -> Just Name
  "need" -> Just Need
  _ -> Nothing

type Store = (Int, Def)

type Def = Map Var (Exp, Bool)

initialStore :: Store
initialStore = (0, Map.empty)

-------------------------- Evaluator Functions for Exp --------------------------

-- | Gets the free variables from an expression
getFreeVars :: Exp -> Set Var
getFreeVars exp = case exp of
  Var v -> Set.singleton v
  Fun v e -> Set.filter (/= v) (getFreeVars e)
  App e1 e2 -> Set.union (getFreeVars e1) (getFreeVars e2)
  IntE _ -> Set.empty
  BoolE _ -> Set.empty
  BopE _ e1 e2 -> Set.union (getFreeVars e1) (getFreeVars e2)
  UopE _ e -> getFreeVars e

-- | Increment the counter in the store and return updated counter
incr :: State Store Int
incr = do
  (x, d) <- S.get
  S.put (x + 1, d)
  return $ x + 1

-- | Generate a fresh variable that does not exist in the store
getFreshVar :: Var -> Set Var -> State Store Var
getFreshVar v freeVars = do
  nextCount <- incr
  let newVar = v ++ show nextCount
  if Set.member newVar freeVars then getFreshVar v freeVars else return newVar

-- | Capture-avoiding-substitute every var v'' with vExp in exp. Alpha convert
-- bound variables as expression is traversed
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

-- | Evaluates a substituted expression given a specific store
evalSubstitute :: Var -> Exp -> Exp -> Store -> Exp
evalSubstitute v vExp exp = S.evalState (substitute v vExp exp)

-- | Returns the weak head normal form of an expression by simplifying
-- lambda terms lazily until it gets stuck on a variable application
whnf :: Exp -> State Store Exp
whnf exp = case exp of
  Var v -> do
    (_, def) <- S.get
    case Map.lookup v def of
      Nothing -> return exp
      Just (e, _) -> whnf e
  App e1 e2 -> do
    rec_call <- whnf e1
    case rec_call of
      Fun v e -> do
        m <- substitute v e2 e
        whnf m
      _ -> return $ App e1 e2
  _ -> return exp

-- | Evaluate a binary operation expression
evalBop :: Bop -> Exp -> Exp -> Exp
evalBop Plus (IntE x) (IntE y) = IntE $ x + y
evalBop Minus (IntE x) (IntE y) = IntE $ x - y
evalBop Times (IntE x) (IntE y) = IntE $ x * y
evalBop Divide (IntE x) (IntE 0) = BopE Divide (IntE x) (IntE 0)
evalBop Divide (IntE x) (IntE y) = IntE $ x `div` y
evalBop Modulo (IntE x) (IntE 0) = BopE Modulo (IntE x) (IntE 0)
evalBop Modulo (IntE x) (IntE y) = IntE $ x `mod` y
evalBop Eq x y = BoolE $ x == y
evalBop Gt (IntE x) (IntE y) = BoolE $ x > y
evalBop Ge (IntE x) (IntE y) = BoolE $ x >= y
evalBop Lt (IntE x) (IntE y) = evalBop Gt (IntE y) (IntE x)
evalBop Le (IntE x) (IntE y) = evalBop Ge (IntE y) (IntE x)
evalBop o e1 e2 = BopE o e1 e2

-- | Evaluate a unary operation expression
evalUop :: Uop -> Exp -> Exp
evalUop Neg (IntE i) = IntE (-i)
evalUop Not (IntE i) = if i > 0 then IntE 0 else IntE 1
evalUop Not (BoolE b) = BoolE (not b)
evalUop o e = UopE o e

-- | Reduce a variable in the store
reduceVar :: EvaluationType -> Var -> (EvaluationType -> Exp -> State Store Exp) -> State Store Exp
reduceVar ct v reduceFun = do
  (count, def) <- S.get
  case Map.lookup v def of
    Nothing -> return (Var v)
    Just (e, reduced) -> do
      case (ct, reduced) of
        (Name, _) -> reduceFun ct e
        (Need, True) -> return e
        (Need, False) -> do
          reducedExp <- reduceFun ct e
          S.put (count, Map.insert v (reducedExp, True) def)
          return reducedExp

-- | Evaluates/simplies the expression through beta reduction
betaReduce :: EvaluationType -> Exp -> State Store Exp
betaReduce ct exp = case exp of
  Fun v e -> Fun v <$> betaReduce ct e
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

-- | Beta-reduce an expression given an evaluation type and store
evalBetaReduce :: EvaluationType -> Exp -> Store -> Exp
evalBetaReduce ct exp = S.evalState (betaReduce ct exp)

-- | Eta-reduce an expression given an evaluation type
etaReduce :: EvaluationType -> Exp -> State Store Exp
etaReduce ct exp = case exp of
  Fun v (App (Fun v' body) (Var x)) ->
    if v == x
      then if Set.member v (getFreeVars body) then return exp else return $ Fun v' body
      else return exp
  Var v -> reduceVar ct v etaReduce
  _ -> return exp

-- | Eta-reduce an expression given an evaluation type and store
evalEtaReduce :: EvaluationType -> Exp -> Store -> Exp
evalEtaReduce ct exp = S.evalState (etaReduce ct exp)

-- | Reduce an expression given a reduction type and evaluation type
reduce :: ReductionType -> EvaluationType -> Exp -> State Store Exp
reduce Beta ct exp = betaReduce ct exp
reduce Eta ct exp = etaReduce ct exp
reduce BetaEta ct exp = etaReduce ct =<< betaReduce ct exp

-- | Reduce an expression given a reduction type, evaluation type, and store
evalReduce :: ReductionType -> EvaluationType -> Exp -> Store -> (Exp, Store)
evalReduce rt ct exp = S.runState (reduce rt ct exp)

------------------------ Evaluator Functions for Assign ------------------------

-- | Return true if var is in the map of declarations, false ow
inDef :: Var -> State Store Bool
inDef v = do
  (_, def) <- S.get
  return $ Map.member v def

-- | Substitute all variables in a given set with the expression they are mapped to
substituteAll :: Set Var -> Def -> Exp -> State Store Exp
substituteAll frees currDefs exp = Set.foldr comb (return exp) frees
  where
    comb :: Var -> State Store Exp -> State Store Exp
    comb free acc = case Map.lookup free currDefs of
      Nothing -> acc
      Just (e, reduced) -> substitute free e =<< acc

-- | Add a variable to the map of definitions using call by name
addDef :: Var -> Exp -> State Store ()
addDef var exp = do
  let frees = getFreeVars exp
  freesInDef <- List.foldr (\v acc -> (&&) <$> inDef v <*> acc) (return True) frees
  if freesInDef
    then do
      (amt, currDefs) <- S.get
      exp' <- substituteAll frees currDefs exp
      S.put (amt, Map.insert var (exp', False) currDefs)
    else error "free var exists in exp but is not defined"

-- | Returns the store after adding specified variable
evalAddDef :: Var -> Exp -> Store -> Store
evalAddDef v exp = S.execState (addDef v exp)