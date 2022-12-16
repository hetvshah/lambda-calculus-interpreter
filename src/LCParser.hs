module LCParser (parseLCStat) where

import Control.Applicative (Alternative (many, some, (<|>)))
import Data.Char qualified as Char
import Data.Functor (($>))
import LCSyntax
  ( Bop (..),
    Exp (..),
    Statement (..),
    Uop (..),
    Var,
    level,
  )
import Parser (Parser)
import Parser qualified as P

-- | Returns a parser for white spaces
wsP :: Parser a -> Parser a
wsP p = p <* many P.space

-- | Returns a parser for a specific string
stringP :: String -> Parser ()
stringP s = wsP (P.string s $> ())

-- | Returns a parser for parentheses
parens :: Parser a -> Parser a
parens x = P.between (stringP "(") x (stringP ")")

-- | Returns a parser for a string and removes whitespace
constP :: String -> a -> Parser a
constP s x = x <$ wsP (P.string s)

-- | Reserved strings
reserved :: [String]
reserved =
  [ "not",
    "true",
    "false"
  ]

-- | Returns a parser for a variable
varP :: Parser Var
varP =
  P.filter
    ( \s ->
        ( case s of
            [] -> False
            c : _ -> not $ Char.isDigit c
        )
          && s `notElem` reserved
    )
    (wsP (many (P.upper <|> P.lower <|> P.digit <|> P.char '_')))

-- | Returns a parser for an integer
intP :: Parser Exp
intP = IntE <$> wsP P.int

-- | Returns a parser for a boolean
boolP :: Parser Exp
boolP = constP "true" (BoolE True) <|> constP "false" (BoolE False)

-- | Returns a parser for a unary operator
uopP :: Parser Uop
uopP = wsP $ constP "~" Neg <|> constP "not" Not

-- | Returns a parser for a binary operator
bopP :: Parser Bop
bopP =
  constP "+" Plus
    <|> constP "- " Minus
    <|> constP "*" Times
    <|> constP "//" Divide
    <|> constP "%" Modulo
    <|> constP "==" Eq
    <|> constP ">=" Ge
    <|> constP ">" Gt
    <|> constP "<=" Le
    <|> constP "<" Lt

-- | Returns a parser for a function of the form '\arg . body'
funP :: Parser Exp
funP = Fun <$> (stringP "\\" *> varP <* wsP (stringP ".")) <*> fullExpP

-- | Returns a parser for any expression except for App being the outermost constructor
expP :: Parser Exp
expP = compP
  where
    compP = sumP `P.chainl1` opAtLevel (level Gt)
    sumP = prodP `P.chainl1` opAtLevel (level Plus)
    prodP = uopexpP `P.chainl1` opAtLevel (level Times)
    uopexpP =
      baseP
        <|> UopE <$> uopP <*> uopexpP
    baseP =
      funP
        <|> Var <$> varP
        <|> parens fullExpP
        <|> intP
        <|> boolP

-- | Returns a parser for an expression, taking care of when App is the outermost constructor
fullExpP :: Parser Exp
fullExpP = foldl1 App <$> some expP

-- | Returns a parser for a statement (either assignment or expression)
statementP :: Parser Statement
statementP =
  Assign <$> varP <*> (stringP "=" *> fullExpP)
    <|> Expression <$> fullExpP

-- | Parses a lambda calculus statement
parseLCStat :: String -> Either P.ParseError Statement
parseLCStat = P.parse statementP

-- | Returns a parser for an operator at a specified precedence level
opAtLevel :: Int -> Parser (Exp -> Exp -> Exp)
opAtLevel l = BopE <$> P.filter (\x -> level x == l) bopP