module LCSyntax where

import Data.Bool qualified as PP
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP

someFunc :: String
someFunc = "Hello!"

lambda :: String
lambda = "λ"

type Var = String

data Exp
  = Var Var -- local variables
  | Fun Var Exp -- functions: fun x -> e
  | App Exp Exp -- applications
  | Int Int -- integers
  | Bool Bool -- booleans
  | BopE Bop Exp Exp -- binary operations
  | UopE Uop Exp -- unary operations
  deriving (Eq, Show)

data Uop
  = Neg -- `-` :: Int -> Int
  | Not -- `not` :: a -> Bool
  deriving (Eq, Show, Enum, Bounded)

data Bop
  = Plus -- `+`  :: Int -> Int -> Int
  | Minus -- `-`  :: Int -> Int -> Int
  | Times -- `*`  :: Int -> Int -> Int
  | Divide -- `//` :: Int -> Int -> Int   -- floor division
  | Modulo -- `%`  :: Int -> Int -> Int   -- modulo
  | Eq -- `==` :: a -> a -> Bool
  | Gt -- `>`  :: a -> a -> Bool
  | Ge -- `>=` :: a -> a -> Bool
  | Lt -- `<`  :: a -> a -> Bool
  | Le -- `<=` :: a -> a -> Bool
  deriving (Eq, Show, Enum, Bounded)

level :: Bop -> Int
level Times = 7
level Divide = 7
level Plus = 5
level Minus = 5
level _ = 3 -- comparison operators

class PP a where
  pp :: a -> Doc

-- | Default operation for the pretty printer. Displays using standard formatting
-- rules, with generous use of indentation and newlines.
pretty :: PP a => a -> String
pretty = PP.render . pp

-- | Compact version. Displays its argument without newlines.
oneLine :: PP a => a -> String
oneLine = PP.renderStyle (PP.style {PP.mode = PP.OneLineMode}) . pp

isValue :: Exp -> Bool
isValue exp = case exp of
  Var _ -> True
  Int _ -> True
  Bool _ -> True
  _ -> False

ppp :: Exp -> Doc
ppp e
  | isValue e = pp e
  | otherwise = PP.parens $ pp e

instance PP String where
  pp :: String -> Doc
  pp = PP.text

instance PP Bool where
  pp True = PP.text "true"
  pp False = PP.text "false"

instance PP Exp where
  pp :: Exp -> Doc
  pp (Var v) = pp v
  pp (Int i) = PP.int i
  pp (Bool b) = pp b
  pp (Fun v e) = PP.text (lambda <> " " <> v <> " . ") <> ppp e
  pp (App e1 e2) = ppp e1 <> PP.char ' ' <> ppp e2
  pp (BopE b e1 e2) = ppp e1 <> PP.text " " <> pp b <> PP.text " " <> ppp e2
  pp (UopE Not e) = pp Not <> PP.text " " <> ppp e
  pp (UopE Neg e) = pp Neg <> ppp e

instance PP Uop where
  pp :: Uop -> Doc
  pp Neg = PP.char '-'
  pp Not = PP.text "not"

instance PP Bop where
  pp :: Bop -> Doc
  pp Plus = PP.char '+'
  pp Minus = PP.char '-'
  pp Times = PP.char '*'
  pp Divide = PP.text "//"
  pp Modulo = PP.text "%"
  pp Gt = PP.char '>'
  pp Ge = PP.text ">="
  pp Lt = PP.char '<'
  pp Le = PP.text "<="
  pp Eq = PP.text "=="

-- >>> pp ex2
-- fun t . (fun f . (t))
