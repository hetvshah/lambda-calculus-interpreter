import Data.Set (Set)
import Data.Set qualified as Set
import Evaluator -- (getArgs, getFreeVars)
import Lib (Bop (..), Exp (..), Uop (..), Var, someFunc)
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC

main :: IO ()
main = do
  putStrLn someFunc
  putStrLn "Test suite not yet implemented"

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

test_alphaConverter :: Test
test_alphaConverter =
  "alphaConverter tests"
    ~: TestList
      [ alphaConverter ex1 ~?= ex1,
        alphaConverter ex2 ~?= ex2,
        alphaConverter ex3 ~?= App (Fun "x1" (Var "x1")) (Var "x"),
        alphaConverter ex4 ~?= ex4,
        alphaConverter ex5 ~?= ex5,
        alphaConverter ex6 ~?= ex6
      ]

prop_freeVarsRemainFree :: Exp -> Bool
prop_freeVarsRemainFree exp = getFreeVars exp == getFreeVars (alphaConverter exp)

prop_argsAndFreeVarsDisjoint :: Exp -> Bool
prop_argsAndFreeVarsDisjoint exp = Set.disjoint (getArgs exp) (getFreeVars exp)

prop_alphaConverter :: Exp -> Bool
prop_alphaConverter exp = alphaConverter exp == alphaConverter (alphaConverter exp)

test_substitute :: Test
test_substitute =
  "substitute tests"
    ~: TestList
      [ substitute "x" (Int 1) ex1 ~?= Fun "x" (Int 1),
        substitute "y" (Bool False) ex1 ~?= Fun "x" (Var "x"),
        substitute "f" (Int 0) ex2 ~?= Fun "t" (Fun "f" (Var "t")),
        substitute "t" (Bool True) ex2 ~?= Fun "t" (Fun "f" (Bool True)),
        substitute "x" (Int 6) ex3 ~?= App (Fun "x" (Int 6)) (Int 6),
        substitute "y" (Int 5) ex4 ~?= App (Fun "x" (Var "x")) (Int 5),
        substitute "x" (Bool False) ex4 ~?= App (Fun "x" (Bool False)) (Var "y"),
        substitute "y" (Int 2) ex5 ~?= Fun "x" (BopE Plus (Int 3) (Int 4)),
        substitute "x" (Bool False) ex5 ~?= Fun "x" (BopE Plus (Int 3) (Int 4)),
        substitute "y" (Var "x") ex6 ~?= Fun "x" (BopE Divide (Var "x") (Var "z")),
        substitute "z" (Bool True) ex6 ~?= Fun "x" (BopE Divide (Var "y") (Bool True))
      ]

prop_substituteAllButArgs :: Var -> Exp -> Exp -> Bool
prop_substituteAllButArgs v vExp exp =
  let isVarInSubbed = Set.member v (getVars (substitute v vExp exp))
   in isVarInSubbed && Set.member v (getArgs exp) || not isVarInSubbed

prop_substituteTwice :: Var -> Exp -> Exp -> Bool
prop_substituteTwice v vExp exp = substitute v vExp exp == substitute v vExp (substitute v vExp exp)

test_betaReducer :: Test
test_betaReducer =
  "betaReducer tests"
    ~: TestList
      [ betaReducer ex1 ~?= ex1,
        betaReducer ex2 ~?= ex2,
        betaReducer ex3 ~?= Var "x",
        betaReducer ex4 ~?= Var "y",
        betaReducer ex5 ~?= ex5,
        betaReducer (App ex5 (Var "y")) ~?= Int 7,
        betaReducer ex6 ~?= ex6,
        betaReducer (App ex6 (Int 1)) ~?= Int 1
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
        etaConverter (App ex5 (Var "y")) ~?= Int 7,
        etaConverter ex6 ~?= ex6,
        etaConverter ex7 ~?= Fun "M" (Var "M's Body")
      ]

prop_etaCorrect :: Exp -> Bool
prop_etaCorrect exp = betaReducer exp == betaReducer (etaConverter exp)

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
