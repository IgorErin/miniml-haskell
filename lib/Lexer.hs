-- https://github.com/AzimMuradov/miniml-compiler-haskell-spbu/blob/master/lib/Parser/Lexer.hs
{-# LANGUAGE OverloadedStrings #-}

module Lexer
  ( Parser,
    sc,
    lexeme,
    symbol,
    colon,
    semicolon2,
    arrow,
    eq,
    leftPar,
    rightPar,
    unitLitP,
    boolLitP,
    intLitP,
    identifierP,
    kwLet,
    kwRec,
    kwIn,
    kwIf,
    kwThen,
    kwElse,
    kwFun,
    kwUnit,
    kwBool,
    kwInt,
  )
where

import Control.Monad (when)
import Data.Int (Int64)
import Data.Text (Text, pack)
import Data.Void (Void)
import Parsetree
import Text.Megaparsec (MonadParsec (..), Parsec, choice, many, (<|>))
import Text.Megaparsec.Char (char, digitChar, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Identifier = String

-- * Basic lexer parts

-- | Parser monad.
type Parser = Parsec Void String

-- | Space consumer, parses whitespace and comments.
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "(*" "*)")

-- | Lexeme, automatically parses trailing whitespace and comments.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Symbol, automatically parses trailing whitespace and comments.
symbol :: String -> Parser String
symbol = L.symbol sc

-- * Symbols

-- | Colon parser.
colon :: Parser String
colon = symbol ":"

-- | Double semicolon parser.
semicolon2 :: Parser String
semicolon2 = symbol ";;"

-- | Arrow parser.
arrow :: Parser String
arrow = symbol "->"

-- | Equality parser.
eq :: Parser String
eq = symbol "="

-- | Left parenthesis parser.
leftPar :: Parser String
leftPar = symbol "("

-- | Right parenthesis parser.
rightPar :: Parser String
rightPar = symbol ")"

-- * Literals

-- | Unit literal parser.
unitLitP :: Parser String
unitLitP = leftPar <* rightPar

-- | Boolean literal parser.
boolLitP :: Parser Bool
boolLitP = True <$ kwTrue <|> False <$ kwFalse

-- | Decimal integer literal parser.
intLitP :: Parser Int
intLitP = do
  int <- lexeme L.decimal
  when (int > absMax) $
    fail "Error: Integer literal exceeds the range of representable integers of type int64"
  return $ fromInteger int
  where
    absMax = 9223372036854775808

-- * Identifiers and keywords

-- ** Identifier

identifierP :: Parser Identifier
identifierP = notReserved *> identifier
  where
    identifier = lexeme $ do
      first <- letterChar <|> char '_'
      other <- many identifierChar
      return $ first : other
    notReserved =
      notFollowedBy $
        choice [kwLet, kwRec, kwIn, kwIf, kwThen, kwElse, kwFun, kwTrue, kwFalse, kwUnit, kwBool, kwInt]

keyword :: String -> Parser String
keyword ident = lexeme $ string ident <* notFollowedBy identifierChar

identifierChar :: Parser Char
identifierChar = letterChar <|> char '_' <|> digitChar

-- ** Keywords

-- | @let@ keyword parser.
kwLet :: Parser String
kwLet = keyword "let"

-- | @rec@ keyword parser.
kwRec :: Parser String
kwRec = keyword "rec"

-- | @in@ keyword parser.
kwIn :: Parser String
kwIn = keyword "in"

-- | @if@ keyword parser.
kwIf :: Parser String
kwIf = keyword "if"

-- | @then@ keyword parser.
kwThen :: Parser String
kwThen = keyword "then"

-- | @else@ keyword parser.
kwElse :: Parser String
kwElse = keyword "else"

-- | @fun@ keyword parser.
kwFun :: Parser String
kwFun = keyword "fun"

-- | @true@ keyword parser.
kwTrue :: Parser String
kwTrue = keyword "true"

-- | @false@ keyword parser.
kwFalse :: Parser String
kwFalse = keyword "false"

-- | @unit@ keyword parser.
kwUnit :: Parser String
kwUnit = keyword "unit"

-- | @bool@ keyword parser.
kwBool :: Parser String
kwBool = keyword "bool"

-- | @int@ keyword parser.
kwInt :: Parser String
kwInt = keyword "int"