import Data.Set (Set)
import Data.Set qualified as Set
import Evaluator
import Lib (Bop (..), Exp (..), Uop (..), Var, someFunc)
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
  putStrLn "----------------------- alphaConverter -----------------------"
  putStrLn "test_alphaConverter"
  -- runTestTT test_alphaConverter
  putStrLn "prop_freeVarsRemainFree"
  -- QC.quickCheck prop_freeVarsRemainFree
  putStrLn "prop_argsAndFreeVarsDisjoint"
  -- QC.quickCheck prop_argsAndFreeVarsDisjoint
  putStrLn "prop_alphaConverter"
  -- QC.quickCheck prop_alphaConverter
  putStrLn "----------------------- substitute -----------------------"
  putStrLn "test_substitute"
  runTestTT test_substitute
  putStrLn "prop_substituteAllButArgs"
  QC.quickCheck prop_substituteAllButArgs
  putStrLn "prop_substituteTwice"
  QC.quickCheck prop_substituteTwice
  putStrLn "----------------------- betaReducer -----------------------"
  putStrLn "test_betaReducer"
  runTestTT test_betaReducer
  putStrLn "prop_noApps"
  QC.quickCheck prop_noApps
  putStrLn "----------------------- etaConverter -----------------------"
  putStrLn "test_etaConverter"
  runTestTT test_etaConverter
  putStrLn "prop_etaCorrect"
  QC.quickCheck prop_etaCorrect
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
ex5 = Fun "x" (BopE Plus (Int 3) (Int 4))

-- λ x . (y / z)
ex6 :: Exp
ex6 = Fun "x" (BopE Divide (Var "y") (Var "z"))

-- λ x . ((λ m . m body) x) --> (λ m . m body)
ex7 :: Exp
ex7 = Fun "x" (App (Fun "M" (Var "M's Body")) (Var "x"))

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

-- test_alphaConverter :: Test
-- test_alphaConverter =
--   "alphaConverter tests"
--     ~: TestList
--       [ alphaConverter ex1 ~?= ex1,
--         alphaConverter ex2 ~?= ex2,
--         alphaConverter ex3 ~?= App (Fun "x1" (Var "x1")) (Var "x"),
--         alphaConverter ex4 ~?= ex4,
--         alphaConverter ex5 ~?= ex5,
--         alphaConverter ex6 ~?= ex6
--       ]

-- prop_freeVarsRemainFree :: Exp -> Bool
-- prop_freeVarsRemainFree exp = getFreeVars exp == getFreeVars (alphaConverter exp)

-- prop_argsAndFreeVarsDisjoint :: Exp -> Bool
-- prop_argsAndFreeVarsDisjoint exp = Set.disjoint (getArgs exp) (getFreeVars exp)

-- prop_alphaConverter :: Exp -> Bool
-- prop_alphaConverter exp = alphaConverter exp == alphaConverter (alphaConverter exp)

initialStore :: Store
initialStore = 0

test_substitute :: Test
test_substitute =
  "substitute tests"
    ~: TestList
      [ evalSubstitute "x" (Int 1) ex1 initialStore ~?= ex1,
        evalSubstitute "y" (Bool False) ex1 initialStore ~?= ex1,
        evalSubstitute "f" (Int 0) ex2 initialStore ~?= ex2,
        evalSubstitute "t" (Bool True) ex2 initialStore ~?= ex2,
        evalSubstitute "x" (Int 6) ex3 initialStore ~?= App (Fun "x" (Var "x")) (Int 6),
        evalSubstitute "y" (Int 5) ex4 initialStore ~?= App (Fun "x" (Var "x")) (Int 5),
        evalSubstitute "x" (Bool False) ex4 initialStore ~?= ex4,
        evalSubstitute "y" (Int 2) ex5 initialStore ~?= ex5,
        evalSubstitute "x" (Bool False) ex5 initialStore ~?= Fun "x" (BopE Plus (Int 3) (Int 4)),
        evalSubstitute "y" (Var "x") ex6 initialStore ~?= Fun "x1" (BopE Divide (Var "x") (Var "z")),
        evalSubstitute "z" (Bool True) ex6 initialStore ~?= Fun "x" (BopE Divide (Var "y") (Bool True))
      ]

prop_substituteAllButArgs :: Var -> Exp -> Exp -> Bool
prop_substituteAllButArgs v vExp exp =
  let isVarInSubbed = Set.member v (getVars (evalSubstitute v vExp exp initialStore))
   in isVarInSubbed && Set.member v (getArgs exp) || not isVarInSubbed

prop_substituteTwice :: Var -> Exp -> Exp -> Bool
prop_substituteTwice v vExp exp = evalSubstitute v vExp exp initialStore == evalSubstitute v vExp (evalSubstitute v vExp exp initialStore) initialStore

test_betaReducer :: Test
test_betaReducer =
  "betaReducer tests"
    ~: TestList
      [ evalBetaReducer ex1 initialStore ~?= ex1,
        evalBetaReducer ex2 initialStore ~?= ex2,
        evalBetaReducer ex3 initialStore ~?= Var "x",
        evalBetaReducer ex4 initialStore ~?= Var "y",
        evalBetaReducer ex5 initialStore ~?= Fun "x" (Int 7),
        evalBetaReducer (App ex5 (Var "y")) initialStore ~?= Int 7,
        evalBetaReducer ex6 initialStore ~?= ex6,
        evalBetaReducer (App ex6 (Int 1)) initialStore ~?= BopE Divide (Var "y") (Var "z")
      ]

prop_noApps :: Exp -> Bool
prop_noApps exp = case exp of
  Fun _ e -> prop_noApps e
  App e1 e2 -> False
  BopE _ e1 e2 -> prop_noApps e1 && prop_noApps e2
  UopE _ e -> prop_noApps e
  _ -> True

test_etaConverter :: Test
test_etaConverter =
  "etaConverter tests"
    ~: TestList
      [ etaConverter ex1 ~?= ex1,
        etaConverter ex2 ~?= ex2,
        etaConverter ex3 ~?= ex3,
        etaConverter ex4 ~?= ex4,
        etaConverter ex5 ~?= ex5,
        etaConverter (App ex5 (Var "y")) ~?= App ex5 (Var "y"),
        etaConverter ex6 ~?= ex6,
        etaConverter ex7 ~?= Fun "M" (Var "M's Body")
      ]

prop_etaCorrect :: Exp -> Bool
prop_etaCorrect exp = evalBetaReducer exp initialStore == evalBetaReducer (etaConverter exp) initialStore

instance Arbitrary Uop where
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary Bop where
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary Exp where
  arbitrary = undefined
  shrink = undefined

--   arbitrary = QC.sized genExp
--   shrink (Val v) = Val <$> shrink v
--   shrink (Var v) = Var <$> shrink v
--   shrink (Op1 o e) = e : [Op1 o e' | e' <- shrink e]
--   shrink (Op2 e1 o e2) =
--     [Op2 e1' o e2 | e1' <- shrink e1]
--       ++ [Op2 e1 o e2' | e2' <- shrink e2]
--       ++ [e1, e2]
--   shrink (TableConst fs) = concatMap getExp fs ++ (TableConst <$> shrink fs)

-- -- | Generate a size-controlled expression
-- genExp :: Int -> Gen Expression
-- genExp 0 = QC.oneof [Var <$> genVar 0, Val <$> arbitrary]
-- genExp n =
--   QC.frequency
--     [ (1, Var <$> genVar n),
--       (1, Val <$> arbitrary),
--       (n, Op1 <$> arbitrary <*> genExp n'),
--       (n, Op2 <$> genExp n' <*> arbitrary <*> genExp n'),
--       (n', TableConst <$> genTableFields n')
--     ]
--   where
--     n' = n `div` 2
