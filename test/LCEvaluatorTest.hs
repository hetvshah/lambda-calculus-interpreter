import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import LCEvaluator
  ( EvaluationType (..),
    ReductionType (..),
    Store,
    evalAddDef,
    evalBetaReduce,
    evalEtaReduce,
    evalReduce,
    evalSubstitute,
    getFreeVars,
    initialStore,
  )
import LCSyntax
  ( Bop (Divide, Plus),
    Exp (..),
    Statement (..),
    Var,
  )
import Test.HUnit (Test (..), runTestTT, (~:), (~?=))
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC

main :: IO ()
main = do
  putStrLn "----------------------- getFreeVars -----------------------"
  putStrLn "test_getFreeVars"
  runTestTT test_getFreeVars
  putStrLn "prop_freeOrArg"
  QC.quickCheck prop_freeOrArg
  putStrLn "----------------------- substitute -----------------------"
  putStrLn "test_substitute"
  runTestTT test_substitute
  putStrLn "prop_substituteAllButArgs"
  QC.quickCheck prop_substituteAllButArgs
  putStrLn "prop_substituteTwice"
  QC.quickCheck prop_substituteTwice
  putStrLn "----------------------- avoidCapture -----------------------"
  putStrLn "test_avoidCapture"
  runTestTT test_avoidCapture
  putStrLn "----------------------- betaReduce -----------------------"
  putStrLn "test_betaReduce"
  runTestTT test_betaReduce
  putStrLn "prop_betaNoRedux"
  QC.quickCheck prop_betaNoRedux
  putStrLn "prop_betaReduceTwice"
  QC.quickCheck prop_betaReduceTwice
  putStrLn "prop_betaFreeWasFree"
  QC.quickCheck prop_betaFreeWasFree
  putStrLn "----------------------- etaReduce -----------------------"
  putStrLn "test_etaReduce"
  runTestTT test_etaReduce
  putStrLn "prop_etaReduceTwice"
  QC.quickCheck prop_etaReduceTwice
  putStrLn "prop_etaFreeWasFree"
  QC.quickCheck prop_etaFreeWasFree
  putStrLn "----------------------- reduce -----------------------"
  putStrLn "test_reduce"
  runTestTT test_reduce
  putStrLn "prop_reduceBeta"
  QC.quickCheck prop_reduceBeta
  putStrLn "prop_reduceEta"
  QC.quickCheck prop_reduceEta
  putStrLn "prop_reduceBetaEta"
  QC.quickCheck prop_reduceBetaEta
  putStrLn "prop_callByNeedOnceIsCallByName"
  QC.quickCheck prop_callByNeedOnceIsCallByName
  -- putStrLn "prop_callByNeedIsReducedInMap"
  -- QC.quickCheck prop_callByNeedIsReducedInMap
  putStrLn "----------------------- addDef -----------------------"
  putStrLn "test_addDef"
  runTestTT test_addDef
  putStrLn "prop_isInStore"
  QC.quickCheck prop_isInStore
  putStrLn "nice work ツ"

-- λ x . x
ex1 :: Exp
ex1 = Fun "x" (Var "x")

-- λ t . (λ f . t)
ex2 :: Exp
ex2 = Fun "t" (Fun "f" (Var "t"))

-- (λ x . x) x
ex3 :: Exp
ex3 = App (Fun "x" (Var "x")) (Var "x")

-- (λ x . x) y
ex4 :: Exp
ex4 = App (Fun "x" (Var "x")) (Var "y")

-- λ x . (3 + 4)
ex5 :: Exp
ex5 = Fun "x" (BopE Plus (IntE 3) (IntE 4))

-- λ x . (y / z)
ex6 :: Exp
ex6 = Fun "x" (BopE Divide (Var "y") (Var "z"))

-- λ x . ((λ m . m body) x) --> (λ m . m body)
ex7 :: Exp
ex7 = Fun "x" (App (Fun "M" (Var "M's Body")) (Var "x"))

exInfinite :: Exp
exInfinite = App (Fun "x" (App (Var "x") (Var "x"))) (Fun "x" (App (Var "x") (Var "x")))

------------------------------ getFreeVars Tests ------------------------------

-- | Gets all variables from an expression
getVars :: Exp -> Set Var
getVars exp = case exp of
  Var v -> Set.singleton v
  Fun v e -> Set.insert v (getVars e)
  App e1 e2 -> Set.union (getVars e1) (getVars e2)
  IntE _ -> Set.empty
  BoolE _ -> Set.empty
  BopE _ e1 e2 -> Set.union (getVars e1) (getVars e2)
  UopE _ e -> getVars e

-- | Gets all the function arguments from an expression
getArgs :: Exp -> Set Var
getArgs exp = case exp of
  Var v -> Set.empty
  Fun v e -> Set.union (Set.singleton v) (getArgs e)
  App e1 e2 -> Set.union (getArgs e1) (getArgs e2)
  IntE _ -> Set.empty
  BoolE _ -> Set.empty
  BopE _ e1 e2 -> Set.union (getArgs e1) (getArgs e2)
  UopE _ e -> getArgs e

test_getFreeVars :: Test
test_getFreeVars =
  "getFreeVars tests"
    ~: TestList
      [ getFreeVars ex1 ~?= (Set.empty :: Set Var),
        getFreeVars ex2 ~?= Set.empty,
        getFreeVars ex3 ~?= Set.singleton "x",
        getFreeVars ex4 ~?= Set.singleton "y",
        getFreeVars ex5 ~?= Set.empty,
        getFreeVars ex6 ~?= Set.fromList ["z", "y"]
      ]

prop_freeOrArg :: Exp -> Bool
prop_freeOrArg exp = getVars exp == Set.union (getArgs exp) (getFreeVars exp)

------------------------------ substitute Tests ------------------------------

test_substitute :: Test
test_substitute =
  "substitute tests"
    ~: TestList
      [ evalSubstitute "x" (IntE 1) ex1 initialStore ~?= ex1,
        evalSubstitute "y" (BoolE False) ex1 initialStore ~?= ex1,
        evalSubstitute "f" (IntE 0) ex2 initialStore ~?= ex2,
        evalSubstitute "t" (BoolE True) ex2 initialStore ~?= ex2,
        evalSubstitute "x" (IntE 6) ex3 initialStore ~?= App (Fun "x" (Var "x")) (IntE 6),
        evalSubstitute "y" (IntE 5) ex4 initialStore ~?= App (Fun "x" (Var "x")) (IntE 5),
        evalSubstitute "x" (BoolE False) ex4 initialStore ~?= ex4,
        evalSubstitute "y" (IntE 2) ex5 initialStore ~?= ex5,
        evalSubstitute "x" (BoolE False) ex5 initialStore ~?= Fun "x" (BopE Plus (IntE 3) (IntE 4)),
        evalSubstitute "y" (Var "x") ex6 initialStore ~?= Fun "x1" (BopE Divide (Var "x") (Var "z")),
        evalSubstitute "z" (BoolE True) ex6 initialStore ~?= Fun "x" (BopE Divide (Var "y") (BoolE True))
      ]

prop_substituteAllButArgs :: Var -> Exp -> Exp -> Bool
prop_substituteAllButArgs v vExp exp =
  let isVarInSubbed = Set.member v (getVars (evalSubstitute v vExp exp initialStore))
   in isVarInSubbed && Set.member v (getArgs exp) || not isVarInSubbed

prop_substituteTwice :: Var -> Exp -> Exp -> Bool
prop_substituteTwice v vExp exp = evalSubstitute v vExp exp initialStore == evalSubstitute v vExp (evalSubstitute v vExp exp initialStore) initialStore

-- (λ y . (λ x. x y)) (λ z . x)
capAv1 :: Exp
capAv1 = App (Fun "y" (Fun "x" (App (Var "x") (Var "y")))) (Fun "z" (Var "x"))

-- λ x1. x1 (λ z . x)
capAv1Expected :: Exp
capAv1Expected = Fun "x1" (App (Var "x1") (Fun "z" (Var "x")))

-- (λ x. (λ y. x)) y
capAv2 :: Exp
capAv2 = App (Fun "x" (Fun "y" (Var "x"))) (Var "y")

-- (λ y1. y)
capAv2Expected :: Exp
capAv2Expected = Fun "y1" (Var "y")

-- (λ f. (λ x. f (f x))) (λ y. y + x)
capAv3 :: Exp
capAv3 = App (Fun "f" (Fun "x" (App (Var "f") (App (Var "f") (Var "x"))))) (Fun "y" (BopE Plus (Var "y") (Var "x")))

-- (λ x1. x1 + x + x)
capAv3Expected :: Exp
capAv3Expected = Fun "x1" (BopE Plus (BopE Plus (Var "x1") (Var "x")) (Var "x"))

-- ((λ y. x) (λ x. x)) x) [y/x]
capAv4 :: Exp
capAv4 = App (App (Fun "y" (Var "x")) (Fun "x" (Var "x"))) (Var "x")

-- (λ y1. y) (λ x. x)) y
capAv4Expected :: Exp
capAv4Expected = App (App (Fun "y1" (Var "y")) (Fun "x" (Var "x"))) (Var "y")

-- λ y . (λ z . λ w . x + y + z + w)
capAv5 :: Exp
capAv5 = Fun "y" (Fun "z" (Fun "w" (BopE Plus (BopE Plus (BopE Plus (Var "x") (Var "y")) (Var "z")) (Var "w"))))

-- λ y . (λ z1 . λ w2 . (λ y . w + z) + y + z1 + w2)
capAv5Expected :: Exp
capAv5Expected = Fun "y" (Fun "z1" (Fun "w2" (BopE Plus (BopE Plus (BopE Plus (Fun "y" (BopE Plus (Var "w") (Var "z"))) (Var "y")) (Var "z1")) (Var "w2"))))

test_avoidCapture :: Test
test_avoidCapture =
  "avoid capture tests"
    ~: TestList
      [ evalBetaReduce Name capAv1 initialStore ~?= capAv1Expected,
        evalBetaReduce Name capAv2 initialStore ~?= capAv2Expected,
        evalBetaReduce Name capAv3 initialStore ~?= capAv3Expected,
        evalSubstitute "x" (Var "y") capAv4 initialStore ~?= capAv4Expected,
        evalSubstitute "x" (Fun "y" (BopE Plus (Var "w") (Var "z"))) capAv5 initialStore ~?= capAv5Expected
      ]

prop_callByNeedOnceIsCallByName :: Exp -> Bool
prop_callByNeedOnceIsCallByName exp = fst (evalReduce Beta Name exp initialStore) == fst (evalReduce Beta Need exp initialStore)

prop_callByNeedIsReducedInMap :: Exp -> Bool
prop_callByNeedIsReducedInMap exp =
  let (reducedExp, (count, newDef)) = evalReduce Beta Need (Var "x") (0, Map.fromList [("x", (exp, False))])
   in case Map.lookup "x" newDef of
        Just (reducedExpInMap, isReduced) ->
          let (reducedTwiceExp, _) = evalReduce Beta Need (Var "x") (count, newDef)
           in isReduced && reducedExp == reducedTwiceExp && reducedExp == reducedExpInMap
        Nothing -> False

------------------------------ betaReduce Tests ------------------------------

test_betaReduce :: Test
test_betaReduce =
  "betaReduce tests"
    ~: TestList
      [ evalBetaReduce Name ex1 initialStore ~?= ex1,
        evalBetaReduce Name ex2 initialStore ~?= ex2,
        evalBetaReduce Name ex3 initialStore ~?= Var "x",
        evalBetaReduce Name ex4 initialStore ~?= Var "y",
        evalBetaReduce Name ex5 initialStore ~?= Fun "x" (IntE 7),
        evalBetaReduce Name (App ex5 (Var "y")) initialStore ~?= IntE 7,
        evalBetaReduce Name ex6 initialStore ~?= ex6,
        evalBetaReduce Name (App ex6 (IntE 1)) initialStore ~?= BopE Divide (Var "y") (Var "z"),
        evalBetaReduce Name ex7 initialStore ~?= Fun "x" (Var "M's Body")
      ]

prop_betaNoRedux :: Exp -> Bool
prop_betaNoRedux exp = case evalBetaReduce Name exp initialStore of
  Fun _ e -> prop_betaNoRedux e
  App (Fun _ _) e2 -> False
  BopE _ e1 e2 -> prop_betaNoRedux e1 && prop_betaNoRedux e2
  UopE _ e -> prop_betaNoRedux e
  _ -> True

prop_betaReduceTwice :: Exp -> Bool
prop_betaReduceTwice exp = evalBetaReduce Name exp initialStore == evalBetaReduce Name (evalBetaReduce Name exp initialStore) initialStore

prop_betaFreeWasFree :: Exp -> Bool
prop_betaFreeWasFree exp = getFreeVars (evalBetaReduce Name exp initialStore) `Set.isSubsetOf` getFreeVars exp

------------------------------ etaReduce Tests ------------------------------

test_etaReduce :: Test
test_etaReduce =
  "etaReduce tests"
    ~: TestList
      [ evalEtaReduce Name ex1 initialStore ~?= ex1,
        evalEtaReduce Name ex2 initialStore ~?= ex2,
        evalEtaReduce Name ex3 initialStore ~?= ex3,
        evalEtaReduce Name ex4 initialStore ~?= ex4,
        evalEtaReduce Name ex5 initialStore ~?= ex5,
        evalEtaReduce Name (App ex5 (Var "y")) initialStore ~?= App ex5 (Var "y"),
        evalEtaReduce Name ex6 initialStore ~?= ex6,
        evalEtaReduce Name ex7 initialStore ~?= Fun "M" (Var "M's Body")
      ]

prop_etaReduceTwice :: Exp -> Bool
prop_etaReduceTwice exp = evalEtaReduce Name exp initialStore == evalEtaReduce Name (evalEtaReduce Name exp initialStore) initialStore

prop_etaFreeWasFree :: Exp -> Bool
prop_etaFreeWasFree exp = getFreeVars (evalEtaReduce Name exp initialStore) `Set.isSubsetOf` getFreeVars exp

------------------------------ Full Reduction Tests ------------------------------

test_reduce :: Test
test_reduce =
  "reduce tests"
    ~: TestList
      [ fst (evalReduce Beta Name ex1 initialStore) ~?= ex1,
        fst (evalReduce Eta Name ex1 initialStore) ~?= ex1,
        fst (evalReduce BetaEta Name ex1 initialStore) ~?= ex1,
        fst (evalReduce Beta Name ex2 initialStore) ~?= ex2,
        fst (evalReduce Eta Name ex2 initialStore) ~?= ex2,
        fst (evalReduce BetaEta Name ex2 initialStore) ~?= ex2,
        fst (evalReduce Beta Name ex3 initialStore) ~?= Var "x",
        fst (evalReduce Eta Name ex3 initialStore) ~?= ex3,
        fst (evalReduce BetaEta Name ex3 initialStore) ~?= Var "x",
        fst (evalReduce Beta Name ex4 initialStore) ~?= Var "y",
        fst (evalReduce Eta Name ex4 initialStore) ~?= ex4,
        fst (evalReduce BetaEta Name ex4 initialStore) ~?= Var "y",
        fst (evalReduce Beta Name ex5 initialStore) ~?= Fun "x" (IntE 7),
        fst (evalReduce Eta Name ex5 initialStore) ~?= ex5,
        fst (evalReduce BetaEta Name ex5 initialStore) ~?= Fun "x" (IntE 7),
        fst (evalReduce Beta Name ex6 initialStore) ~?= ex6,
        fst (evalReduce Eta Name ex6 initialStore) ~?= ex6,
        fst (evalReduce BetaEta Name ex6 initialStore) ~?= ex6,
        fst (evalReduce Beta Name ex7 initialStore) ~?= Fun "x" (Var "M's Body"),
        fst (evalReduce Eta Name ex7 initialStore) ~?= Fun "M" (Var "M's Body"),
        fst (evalReduce BetaEta Name ex7 initialStore) ~?= Fun "x" (Var "M's Body")
      ]

prop_reduceBeta :: Exp -> Bool
prop_reduceBeta exp = fst (evalReduce Beta Name exp initialStore) == evalBetaReduce Name exp initialStore

prop_reduceEta :: Exp -> Bool
prop_reduceEta exp = fst (evalReduce Eta Name exp initialStore) == evalEtaReduce Name exp initialStore

prop_reduceBetaEta :: Exp -> Bool
prop_reduceBetaEta exp = fst (evalReduce BetaEta Name exp initialStore) == evalEtaReduce Name (evalBetaReduce Name exp initialStore) initialStore

------------------------------ addDef Tests ------------------------------
notReduced :: Exp -> (Exp, Bool)
notReduced e = (e, False)

initialStore' :: Store
initialStore' = (0, Map.fromList [("x", notReduced $ IntE 7), ("y", notReduced $ IntE 3), ("z", notReduced $ IntE 1), ("M's Body", notReduced $ IntE 3)])

test_addDef :: Test
test_addDef =
  "addDef tests"
    ~: TestList
      [ snd (evalAddDef "v" ex1 initialStore) ~?= Map.singleton "v" (notReduced ex1),
        snd (evalAddDef "v" ex2 initialStore) ~?= Map.singleton "v" (notReduced ex2),
        snd (evalAddDef "v" ex5 (evalAddDef "u" ex3 (evalAddDef "v" ex4 initialStore')))
          ~?= Map.fromList [("M's Body", notReduced $ IntE 3), ("u", notReduced $ App (Fun "x" (Var "x")) (IntE 7)), ("v", notReduced $ Fun "x" (BopE Plus (IntE 3) (IntE 4))), ("x", notReduced $ IntE 7), ("y", notReduced $ IntE 3), ("z", notReduced $ IntE 1)],
        snd (evalAddDef "v" ex6 initialStore')
          ~?= Map.fromList [("M's Body", notReduced $ IntE 3), ("v", notReduced $ Fun "x" (BopE Divide (IntE 3) (IntE 1))), ("x", notReduced $ IntE 7), ("y", notReduced $ IntE 3), ("z", notReduced $ IntE 1)],
        snd (evalAddDef "v" ex7 initialStore')
          ~?= Map.fromList [("M's Body", notReduced $ IntE 3), ("v", notReduced $ Fun "x" (App (Fun "M" (IntE 3)) (Var "x"))), ("x", notReduced $ IntE 7), ("y", notReduced $ IntE 3), ("z", notReduced $ IntE 1)]
      ]

prop_isInStore :: Var -> Exp -> QC.Property
prop_isInStore var exp =
  null (getFreeVars exp)
    QC.==> let (def, new_store) = evalAddDef var exp initialStore
            in Map.member var new_store