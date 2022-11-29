module LCParser where

import Control.Applicative
import Data.Functor (($>))
import Lib
import Parser (Parser)
import Parser qualified as P

wsP :: Parser a -> Parser a
wsP p = p <* many P.space

stringP :: String -> Parser ()
stringP s = wsP (P.string s $> ())

parens :: Parser a -> Parser a
parens x = P.between (stringP "(") x (stringP ")")

dotP :: Parser a -> Parser a
dotP = undefined

varP :: Parser Var
varP = undefined

intP :: Parser Exp
intP = undefined

boolP :: Parser Exp
boolP = undefined

uopP :: Parser Uop
uopP = undefined

bopP :: Parser Bop
bopP = undefined

funP :: Parser Exp
funP = undefined

appP :: Parser Exp
appP = undefined

expP :: Parser Exp
expP = undefined

-- parseLCFile :: String -> IO (Either P.ParseError Exp)
-- parseLCFile = P.parseFromFile (const <$> expP <*> P.eof)
