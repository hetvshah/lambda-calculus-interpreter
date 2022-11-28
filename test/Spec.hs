import Data.Set (Set)
import Data.Set qualified as Set
import Evaluator (getFreeVarsFromExp)
import Lib (Exp (..), Var, someFunc)
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

-- >>> getFreeVarsFromExp ex1

test_getFreeVarsFromExp :: Test
test_getFreeVarsFromExp =
  "getFreeVarsFromExp tests"
    ~: TestList
      [ getFreeVarsFromExp ex1 ~?= Set.empty,
        getFreeVarsFromExp ex2 ~?= Set.empty,
        getFreeVarsFromExp ex3 ~?= Set.singleton (Var "x"),
        getFreeVarsFromExp ex4 ~?= Set.singleton (Var "y")
      ]

-- instance Arbitrary UopE where
--   arbitrary = QC.arbitraryBoundedEnum

-- instance Arbitrary BopE where
--   arbitrary = QC.arbitraryBoundedEnum

-- instance Arbitrary Exp where
--   arbitrary = undefined
--   shrink = undefined

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
