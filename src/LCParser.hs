module LCParser where

import Control.Applicative
import Data.Char qualified as Char
import Data.Functor (($>))
import LCSyntax
import Parser (Parser)
import Parser qualified as P

wsP :: Parser a -> Parser a
wsP p = p <* many P.space

stringP :: String -> Parser ()
stringP s = wsP (P.string s $> ())

parens :: Parser a -> Parser a
parens x = P.between (stringP "(") x (stringP ")")

constP :: String -> a -> Parser a
constP s x = x <$ wsP (P.string s)

reserved :: [String]
reserved =
  [ "not",
    "true",
    "false"
  ]

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

intP :: Parser Exp
intP = Int <$> wsP P.int

boolP :: Parser Exp
boolP = constP "true" (Bool True) <|> constP "false" (Bool False)

uopP :: Parser Uop
uopP = wsP $ constP "-" Neg <|> constP "not" Not

bopP :: Parser Bop
bopP =
  constP "+" Plus
    <|> constP "-" Minus
    <|> constP "*" Times
    <|> constP "//" Divide
    <|> constP "%" Modulo
    <|> constP "==" Eq
    <|> constP ">=" Ge
    <|> constP ">" Gt
    <|> constP "<=" Le
    <|> constP "<" Lt

funP :: Parser Exp
funP = Fun <$> (stringP "\\" *> varP <* wsP (stringP ".")) <*> fullExpP

-- z + x * x + y
-- (\x . x + x) y + y
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

fullExpP :: Parser Exp
fullExpP = foldl1 App <$> some expP

-- | Parse an operator at a specified precedence level
opAtLevel :: Int -> Parser (Exp -> Exp -> Exp)
opAtLevel l = BopE <$> P.filter (\x -> level x == l) bopP

parseLCExp :: String -> Either P.ParseError Exp
parseLCExp = P.parse fullExpP