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
  putStrLn someFunc
  putStrLn "Test suite not yet implemented"

prop_roundtrip_exp :: Exp -> Bool
prop_roundtrip_exp e = P.parse expP (pretty e) == Right e

test_varP :: Test
test_varP =
  "parsing vars"
    ~: TestList
      [ P.parse varP "    " ~?= Left "No parses",
        P.parse (many varP) "x sfds _" ~?= Right ["x", "sfds", "_"],
        P.parse (many varP) "843829 3842 2 3 4 2" ~?= Left "No parses",
        P.parse (many varP) "x    y  " ~?= Right ["x", "y"],
        P.parse varP "x    y  " ~?= Right "x"
      ]

test_intP =
  "parsing ints"
    ~: TestList
      [ P.parse intP "     " ~?= Left "No parses",
        P.parse intP "1     " ~?= Right (Int 1),
        P.parse intP "2 4     " ~?= Right (Int 2),
        P.parse (many intP) ".. false \n" ~?= Left "No parses",
        P.parse (many intP) "1 2\n 3" ~?= Right [Int 1, Int 2, Int 3]
      ]

test_boolP =
  "parsing bools"
    ~: TestList
      [ P.parse boolP "    " ~?= Left "No parses",
        P.parse boolP "true    " ~?= Right (Bool True),
        P.parse boolP "false true   " ~?= Right (Bool False),
        P.parse (many boolP) "1 2\n 3" ~?= Left "No parses",
        P.parse (many boolP) "true false\n false" ~?= Right [Bool True, Bool False, Bool False]
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
        P.parse (many bopP) "1 2\n 3" ~?= Left "No parses",
        P.parse (many bopP) "<= +\n -" ~?= Right [Le, Plus, Minus]
      ]

-- P.parse varP "\"a\"" ~?= Right "a",
-- P.parse varP "\"a\\\"\"" ~?= Right "a\\",
-- P.parse (many varP) "\"a\"   \"b\"" ~?= Right ["a", "b"],
-- P.parse (many varP) "\" a\"   \"b\"" ~?= Right [" a", "b"]