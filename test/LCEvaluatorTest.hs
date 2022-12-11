import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import LCEvaluator
import LCSyntax
import Test.HUnit
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
  putStrLn "----------------------- addDef -----------------------"
  putStrLn "test_addDef"
  runTestTT test_addDef
  putStrLn "prop_isInStore"
  QC.quickCheck prop_isInStore
  putStrLn "----------------------- reduce -----------------------"
  putStrLn "test_reduce"
  runTestTT test_reduce
  putStrLn "prop_reduceBeta"
  QC.quickCheck prop_reduceBeta
  putStrLn "prop_reduceEta"
  QC.quickCheck prop_reduceEta
  putStrLn "prop_reduceBetaEta"
  QC.quickCheck prop_reduceBetaEta
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

test_betaReduce :: Test
test_betaReduce =
  "betaReduce tests"
    ~: TestList
      [ evalBetaReduce ex1 initialStore ~?= ex1,
        evalBetaReduce ex2 initialStore ~?= ex2,
        evalBetaReduce ex3 initialStore ~?= Var "x",
        evalBetaReduce ex4 initialStore ~?= Var "y",
        evalBetaReduce ex5 initialStore ~?= Fun "x" (IntE 7),
        evalBetaReduce (App ex5 (Var "y")) initialStore ~?= IntE 7,
        evalBetaReduce ex6 initialStore ~?= ex6,
        evalBetaReduce (App ex6 (IntE 1)) initialStore ~?= BopE Divide (Var "y") (Var "z"),
        evalBetaReduce ex7 initialStore ~?= Fun "x" (Var "M's Body")
      ]

prop_betaNoRedux :: Exp -> Bool
prop_betaNoRedux exp = case evalBetaReduce exp initialStore of
  Fun _ e -> prop_betaNoRedux e
  App (Fun _ _) e2 -> False
  BopE _ e1 e2 -> prop_betaNoRedux e1 && prop_betaNoRedux e2
  UopE _ e -> prop_betaNoRedux e
  _ -> True

prop_betaReduceTwice :: Exp -> Bool
prop_betaReduceTwice exp = evalBetaReduce exp initialStore == evalBetaReduce (evalBetaReduce exp initialStore) initialStore

prop_betaFreeWasFree :: Exp -> Bool
prop_betaFreeWasFree exp = getFreeVars (evalBetaReduce exp initialStore) `Set.isSubsetOf` getFreeVars exp

test_etaReduce :: Test
test_etaReduce =
  "etaReduce tests"
    ~: TestList
      [ etaReduce ex1 ~?= ex1,
        etaReduce ex2 ~?= ex2,
        etaReduce ex3 ~?= ex3,
        etaReduce ex4 ~?= ex4,
        etaReduce ex5 ~?= ex5,
        etaReduce (App ex5 (Var "y")) ~?= App ex5 (Var "y"),
        etaReduce ex6 ~?= ex6,
        etaReduce ex7 ~?= Fun "M" (Var "M's Body")
      ]

prop_etaReduceTwice :: Exp -> Bool
prop_etaReduceTwice exp = etaReduce exp == etaReduce (etaReduce exp)

prop_etaFreeWasFree :: Exp -> Bool
prop_etaFreeWasFree exp = getFreeVars (etaReduce exp) `Set.isSubsetOf` getFreeVars exp

initialStore' :: Store
initialStore' = (0, Map.fromList [("x", IntE 7), ("y", IntE 3), ("z", IntE 1), ("M's Body", IntE 3)])

test_addDef :: Test
test_addDef =
  "addDef tests"
    ~: TestList
      [ snd (evalAddDef "v" ex1 initialStore) ~?= Map.singleton "v" ex1,
        snd (evalAddDef "v" ex2 initialStore) ~?= Map.singleton "v" ex2,
        snd (evalAddDef "v" ex5 (evalAddDef "u" ex3 (evalAddDef "v" ex4 initialStore')))
          ~?= Map.union (Map.fromList [("v", Fun "x1" (IntE 7)), ("u", IntE 7)]) (snd initialStore'),
        snd (evalAddDef "v" ex6 initialStore')
          ~?= Map.fromList [("M's Body", IntE 3), ("v", Fun "x1" (IntE 3)), ("x", IntE 7), ("y", IntE 3), ("z", IntE 1)],
        snd (evalAddDef "v" ex7 initialStore')
          ~?= Map.fromList [("M's Body", IntE 3), ("v", Fun "x1" (IntE 3)), ("x", IntE 7), ("y", IntE 3), ("z", IntE 1)]
      ]

prop_isInStore :: Var -> Exp -> QC.Property
prop_isInStore var exp =
  null (getFreeVars exp)
    QC.==> let (def, new_store) = evalAddDef var exp initialStore
            in Map.member var new_store

test_reduce :: Test
test_reduce =
  "reduce tests"
    ~: TestList
      [ evalReduce Beta ex1 initialStore ~?= ex1,
        evalReduce Eta ex1 initialStore ~?= ex1,
        evalReduce BetaEta ex1 initialStore ~?= ex1,
        evalReduce Beta ex2 initialStore ~?= ex2,
        evalReduce Eta ex2 initialStore ~?= ex2,
        evalReduce BetaEta ex2 initialStore ~?= ex2,
        evalReduce Beta ex3 initialStore ~?= Var "x",
        evalReduce Eta ex3 initialStore ~?= ex3,
        evalReduce BetaEta ex3 initialStore ~?= Var "x",
        evalReduce Beta ex4 initialStore ~?= Var "y",
        evalReduce Eta ex4 initialStore ~?= ex4,
        evalReduce BetaEta ex4 initialStore ~?= Var "y",
        evalReduce Beta ex5 initialStore ~?= Fun "x" (IntE 7),
        evalReduce Eta ex5 initialStore ~?= ex5,
        evalReduce BetaEta ex5 initialStore ~?= Fun "x" (IntE 7),
        evalReduce Beta ex6 initialStore ~?= ex6,
        evalReduce Eta ex6 initialStore ~?= ex6,
        evalReduce BetaEta ex6 initialStore ~?= ex6,
        evalReduce Beta ex7 initialStore ~?= Fun "x" (Var "M's Body"),
        evalReduce Eta ex7 initialStore ~?= Fun "M" (Var "M's Body"),
        evalReduce BetaEta ex7 initialStore ~?= Fun "x" (Var "M's Body")
      ]

prop_reduceBeta :: Exp -> Bool
prop_reduceBeta exp = evalReduce Beta exp initialStore == evalBetaReduce exp initialStore

prop_reduceEta :: Exp -> Bool
prop_reduceEta exp = evalReduce Eta exp initialStore == etaReduce exp

prop_reduceBetaEta :: Exp -> Bool
prop_reduceBetaEta exp = evalReduce BetaEta exp initialStore == etaReduce (evalBetaReduce exp initialStore)
