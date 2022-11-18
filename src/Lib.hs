module Lib where

import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP

someFunc :: String
someFunc = "Hello!"

type Var = String

data Exp
  = Var Var -- local variables
  | Fun Var Exp -- functions: fun x -> e
  | App Exp Exp -- applications
  deriving (Eq, Show)

class PP a where
  pp :: a -> Doc

-- | Default operation for the pretty printer. Displays using standard formatting
-- rules, with generous use of indentation and newlines.
pretty :: PP a => a -> String
pretty = PP.render . pp

-- | Compact version. Displays its argument without newlines.
oneLine :: PP a => a -> String
oneLine = PP.renderStyle (PP.style {PP.mode = PP.OneLineMode}) . pp

instance PP String where
  pp :: String -> Doc
  pp = PP.text

instance PP Exp where
  pp :: Exp -> Doc
  pp (Var v) = pp v
  pp (Fun v e) = PP.text ("fun" <> " " <> v <> " . ") <> PP.parens (pp e)
  pp (App e1 e2) = PP.parens (pp e1) <> PP.char ' ' <> PP.parens (pp e2)

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

-- >>> pp ex2
-- Fun "t" (Fun "f" (Var "t"))
