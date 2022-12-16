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
  putStrLn "test_funP"
  runTestTT test_funP
  putStrLn "test_expP"
  runTestTT test_expP
  putStrLn "test_fullExpP"
  runTestTT test_fullExpP
  putStrLn "test_statementP"
  runTestTT test_statementP
  putStrLn "nice work ツ"

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
      [ P.parse uopP "~   " ~?= Right Neg,
        P.parse uopP "not    " ~?= Right Not,
        P.parse uopP "not -   " ~?= Right Not,
        P.parse (many uopP) "~ \n not ~" ~?= Right [Neg, Not, Neg],
        P.parse (many uopP) "~ ~ ~" ~?= Right [Neg, Neg, Neg]
      ]

test_bopP =
  "parsing bops"
    ~: TestList
      [ P.parse bopP "+   , " ~?= Right Plus,
        P.parse bopP ">    " ~?= Right Gt,
        P.parse bopP ">   / " ~?= Right Gt,
        P.parse (some bopP) "1 2\n 3" ~?= Left "No parses",
        P.parse (many bopP) "<= +\n - " ~?= Right [Le, Plus, Minus]
      ]
  
test_funP = 
  "parsing funs"
  ~: TestList 
      [
        P.parse funP "\\x. x + y" ~?= Right (Fun "x" (BopE Plus (Var "x") (Var "y"))),
        P.parse funP "\\x. " ~?= Left "No parses",
        P.parse funP "\\x. \\y. x * y" ~?= Right (Fun "x" (Fun "y" (BopE Times (Var "x") (Var "y")))),
        P.parse funP "\\x y . x + y" ~?= Left "No parses",
        P.parse funP "\\ x" ~?= Left "No parses",
        P.parse funP "\\ y y" ~?= Left "No parses",
        P.parse funP "\\x               . x + y" ~?= Right (Fun "x" (BopE Plus (Var "x") (Var "y")))
      ]

test_expP = 
  "parsing exps"
  ~: TestList 
    [
      P.parse expP "\\x. x" ~?= Right (Fun "x" (Var "x")),
      P.parse expP "\\t . (\\ f. t)" ~?= Right (Fun "t" (Fun "f" (Var "t"))),
      P.parse expP "(\\x . x) x" ~?= Right (Fun "x" (Var "x")),
      P.parse expP "\\x . (3 + 4)" ~?= Right (Fun "x" (BopE Plus (IntE 3) (IntE 4))), 
      P.parse expP "\\x . (y // z)" ~?= Right (Fun "x" (BopE Divide (Var "y") (Var "z"))),
      P.parse expP "\\x . (\\m . M) x" ~?= Right (Fun "x" (App (Fun "m" (Var "M")) (Var "x"))),
      P.parse expP "\ \ x .    x + y" ~?= Left "No parses",
      P.parse expP "x + y" ~?= Right (BopE Plus (Var "x") (Var "y")),
      P.parse expP "x" ~?= Right (Var "x"),
      P.parse expP "\\ y . (((((((((((((x + y)))))))))))))" ~?= Right (Fun "y" (BopE Plus (Var "x") (Var "y"))),
      P.parse expP "x x" ~?= Right (Var "x"),
      P.parse expP "x // y" ~?= Right (BopE Divide (Var "x") (Var "y")),
      P.parse expP "3" ~?= Right (IntE 3)
    ]

test_fullExpP = 
  "parsing full exps"
  ~: TestList [
      P.parse fullExpP "\\x. x" ~?= Right (Fun "x" (Var "x")),
      P.parse fullExpP "\\t . (\\ f. t)" ~?= Right (Fun "t" (Fun "f" (Var "t"))),
      P.parse fullExpP "(\\x . x) x" ~?= Right (App (Fun "x" (Var "x")) (Var "x")),
      P.parse fullExpP "\\x . (3 + 4)" ~?= Right (Fun "x" (BopE Plus (IntE 3) (IntE 4))), 
      P.parse fullExpP "\\x . (y // z)" ~?= Right (Fun "x" (BopE Divide (Var "y") (Var "z"))),
      P.parse fullExpP "\\x . (\\m . M) x" ~?= Right (Fun "x" (App (Fun "m" (Var "M")) (Var "x"))),
      P.parse fullExpP "\ \ x .    x + y" ~?= Left "No parses",
      P.parse fullExpP "x + y" ~?= Right (BopE Plus (Var "x") (Var "y")),
      P.parse fullExpP "x" ~?= Right (Var "x"),
      P.parse fullExpP "\\ y . (((((((((((((x + y)))))))))))))" ~?= Right (Fun "y" (BopE Plus (Var "x") (Var "y"))),
      P.parse fullExpP "x x" ~?= Right (App (Var "x") (Var "x")),
      P.parse fullExpP "x // y" ~?= Right (BopE Divide (Var "x") (Var "y")),
      P.parse fullExpP "3" ~?= Right (IntE 3)
  ]
  
test_statementP = 
  "parsing statements"
  ~: TestList [
    P.parse statementP "x = 3" ~?= Right (Assign "x" (IntE 3)),
    P.parse statementP "y = \\z . (z + 7) " ~?= Right (Assign "y" (Fun "z" (BopE Plus (Var "z") (IntE 7)))),
    P.parse statementP "False = \\f. \\t. f" ~?= Right (Assign "False" (Fun "f" (Fun "t" (Var "f")))),
    P.parse statementP "fix = \\g . (\\x . g (x x)) (\\x . g (x x))" ~?= Right (Assign "fix" (Fun "g" (App (Fun "x" (App (Var "g") (App (Var "x") (Var "x")))) (Fun "x" (App (Var "g") (App (Var "x") (Var "x"))))))),
    P.parse statementP "add = fix (\\radd.\\x.\\y. x y (\\ n. Succ (radd n y)))" ~?= Right (Assign "add" (App (Var "fix") (Fun "radd" (Fun "x" (Fun "y" (App (App (Var "x") (Var "y")) (Fun "n" (App (Var "Succ") (App (App (Var "radd") (Var "n")) (Var "y")))))))))),
    P.parse statementP "eqnat n720 (add n703 n17)" ~?= Right (Expression (App (App (Var "eqnat") (Var "n720")) (App (App (Var "add") (Var "n703")) (Var "n17"))))
  ]

