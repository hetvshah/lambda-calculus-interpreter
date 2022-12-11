{-# LANGUAGE ImportQualifiedPost #-}

import Control.Applicative
import LCEvaluator
import LCParser
import LCSyntax
import Parser (Parser)
import Parser qualified as P
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC

main :: IO ()
main = do
  putStrLn "prop_roundtrip_exp"
  QC.quickCheck prop_roundtrip_exp
  putStrLn "prop_roundtrip_stmt"
  QC.quickCheck prop_roundtrip_stmt
  putStrLn "test_varP"
  runTestTT test_varP
  putStrLn "test_intP"
  runTestTT test_intP
  putStrLn "test_boolP"
  runTestTT test_boolP
  putStrLn "test_uopP"
  runTestTT test_uopP
  putStrLn "test_bopP"
  runTestTT test_bopP
  putStrLn "nice work ツ"

-- e = UopE Neg (IntE 1)
-- P.parse fullExpP (pretty e)
-- Right e

prop_roundtrip_exp :: Exp -> Bool
prop_roundtrip_exp e = P.parse fullExpP (pretty e) == Right e

prop_roundtrip_stmt :: Statement -> Bool
prop_roundtrip_stmt e = P.parse statementP (pretty e) == Right e

test_varP :: Test
test_varP =
  "parsing vars"
    ~: TestList
      [ P.parse varP "    " ~?= Left "No parses",
        P.parse (many varP) "x sfds _" ~?= Right ["x", "sfds", "_"],
        P.parse (some varP) "843829 3842 2 3 4 2" ~?= Left "No parses",
        P.parse (many varP) "x    y  " ~?= Right ["x", "y"],
        P.parse varP "x    y  " ~?= Right "x"
      ]

test_intP =
  "parsing ints"
    ~: TestList
      [ P.parse intP "     " ~?= Left "No parses",
        P.parse intP "1     " ~?= Right (IntE 1),
        P.parse intP "2 4     " ~?= Right (IntE 2),
        P.parse (some intP) ".. false \n" ~?= Left "No parses",
        P.parse (many intP) "1 2\n 3" ~?= Right [IntE 1, IntE 2, IntE 3]
      ]

test_boolP =
  "parsing bools"
    ~: TestList
      [ P.parse boolP "    " ~?= Left "No parses",
        P.parse boolP "true    " ~?= Right (BoolE True),
        P.parse boolP "false true   " ~?= Right (BoolE False),
        P.parse (some boolP) "1 2\n 3" ~?= Left "No parses",
        P.parse (many boolP) "true false\n false" ~?= Right [BoolE True, BoolE False, BoolE False]
      ]

test_uopP =
  "parsing uops"
    ~: TestList
      [ P.parse uopP "-   " ~?= Right Neg,
        P.parse uopP "not    " ~?= Right Not,
        P.parse uopP "not -   " ~?= Right Not,
        P.parse (many uopP) "- \n not -" ~?= Right [Neg, Not, Neg],
        P.parse (many uopP) "- - -" ~?= Right [Neg, Neg, Neg]
      ]

test_bopP =
  "parsing bops"
    ~: TestList
      [ P.parse bopP "+   , " ~?= Right Plus,
        P.parse bopP ">    " ~?= Right Gt,
        P.parse bopP ">   / " ~?= Right Gt,
        P.parse (some bopP) "1 2\n 3" ~?= Left "No parses",
        P.parse (many bopP) "<= +\n -" ~?= Right [Le, Plus, Minus]
      ]