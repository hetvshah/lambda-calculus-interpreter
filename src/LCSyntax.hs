module LCSyntax where

import Control.Monad (liftM2)
import Data.Bool qualified as PP
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP

lambda :: String
lambda = "\\"

type Var = String

data Exp
  = Var Var -- local variables
  | Fun Var Exp -- functions: fun x -> e
  | App Exp Exp -- applications
  | IntE Int -- integers
  | BoolE Bool -- booleans
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

data Statement
  = Assign Var Exp -- x = e
  | Expression Exp -- some expression to evaluate
  deriving (Eq, Show)

-------------------------------- Arbitrary Definitions --------------------------------

instance Arbitrary Uop where
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary Bop where
  arbitrary = QC.arbitraryBoundedEnum

-- | Generate a small set of names for generated tests. These names are guaranteed to not include
-- reserved words
genVar :: Gen Var
genVar = QC.elements ["x", "X", "y", "x0", "X0", "xy", "XY", "_x"]

-- | Generate a size-controlled expression
genExp :: Int -> Gen Exp
genExp 0 = QC.oneof [Var <$> genVar, IntE <$> arbitrary, BoolE <$> arbitrary]
genExp n =
  QC.frequency
    [ (1, Var <$> genVar),
      (n, Fun <$> genVar <*> genExp n'),
      (n, App <$> genExp n' <*> genExp n'),
      (n, IntE <$> arbitrary),
      (n, BoolE <$> arbitrary),
      (n, UopE <$> arbitrary <*> genExp n'),
      (n, BopE <$> arbitrary <*> genExp n' <*> genExp n')
    ]
  where
    n' = n `div` 2

-- | Generate a size-controlled statement
genStatement :: Int -> Gen Statement
genStatement n | n <= 1 = QC.oneof [Assign <$> genVar <*> genExp 0, Expression <$> genExp 0]
genStatement n =
  QC.frequency
    [ (n, Assign <$> genVar <*> genExp n'),
      (n, Expression <$> genExp n')
    ]
  where
    n' = n `div` 2

instance Arbitrary Exp where
  arbitrary = QC.sized genExp

  shrink (UopE o e) = e : [UopE o e' | e' <- shrink e]
  shrink (BopE o e1 e2) =
    [BopE o e1' e2 | e1' <- shrink e1]
      ++ [BopE o e1 e2' | e2' <- shrink e2]
      ++ [e1, e2]
  shrink (Fun v e) = [Fun v e' | e' <- shrink e]
  shrink (App e1 e2) =
    [App e1' e2 | e1' <- shrink e1]
      ++ [App e1 e2' | e2' <- shrink e2]
      ++ [e1, e2]
  shrink _ = []

instance Arbitrary Statement where
  arbitrary = QC.sized genStatement
  shrink (Assign v e) = [Assign v e' | e' <- shrink e]
  shrink (Expression e) = [Expression e' | e' <- shrink e]

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
  IntE _ -> True
  BoolE _ -> True
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
  pp (IntE i) = PP.int i
  pp (BoolE b) = pp b
  pp (Fun v e) = PP.text (lambda <> " " <> v <> " . ") <> ppp e
  pp (App e1 e2) = ppp e1 <> PP.char ' ' <> ppp e2
  pp (BopE b e1 e2) = ppp e1 <> PP.text " " <> pp b <> PP.text " " <> ppp e2
  pp (UopE Not e) = pp Not <> PP.text " " <> ppp e
  pp (UopE Neg e) = pp Neg <> ppp e

instance PP Uop where
  pp :: Uop -> Doc
  pp Neg = PP.char '~'
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

instance PP Statement where
  pp (Assign x e) = pp x <+> PP.equals <+> pp e
  pp (Expression e) = pp e

data ReductionType = Beta | Eta | BetaEta

reductionToEnum :: String -> ReductionType
reductionToEnum str = case str of
  "beta" -> Beta
  "eta" -> Eta
  "beta-eta" -> BetaEta
  _ -> error "shouldn't come here"

data EvaluationType = Name | Need

evaluationToEnum :: String -> EvaluationType
evaluationToEnum str = case str of
  "name" -> Name
  "need" -> Need
  _ -> error "shouldn't come here"
