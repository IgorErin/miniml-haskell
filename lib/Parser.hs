-- https://github.com/AzimMuradov/miniml-compiler-haskell-spbu/blob/master/lib/Parser/Parser.hs
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use first" #-}

module Parser (parseProgram, parseExpr) where

import Control.Arrow (ArrowChoice (right))
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Bifunctor (bimap)
import Data.List.NonEmpty (some1)
import Data.String (String)
import Lexer
import ParserUtils
import Parsetree
import Text.Megaparsec (MonadParsec (..), choice, many, option, parse, parseMaybe, (<?>))
import Text.Megaparsec.Error (errorBundlePretty)
import Typedtree

type Type = Parsetree.Ty

type Identifier = String

-- | Parser entry point
parseProgram :: String -> Either String Program
parseProgram str =
  bimap errorBundlePretty id $
    (parse $ sc *> programP <* eof) "file.ml" str

parseExpr :: String -> Either String Expr
parseExpr str = bimap errorBundlePretty id foo
  where
    foo = (parse $ sc *> exprP <* eof) "file.ml" str

-- * Internal

programP :: Parser Program
programP = Program <$> many declP

-- stmtP :: Parser Statement
-- stmtP = choice' [StmtExpr <$> exprP, StmtDecl <$> declP]

-- ** User Declaration Parsers

patternP :: Parser Pattern
patternP =
  choice
    [ leftPar
        *> choice
          [ (`PVar` PMUnique) <$> (keyword "unique" *> identifierP <* rightPar),
            patternP <* rightPar,
            PUnit <$ rightPar
          ],
      pvar <$> identifierP,
      PAny <$ (sc *> Lexer.keyword "_")
    ]

declP :: Parser StructureItem
declP = recDecl
  where
    -- varDeclP = DeclVar <$ kwLet <*> varSigP <* eq <*> exprP
    recDecl =
      (\flg pat args body -> SItem flg pat (elams args body))
        <$ kwLet
        <*> option NonRecursive (Recursive <$ (sc *> kwRec))
        <*> patternP
        <*> many (sc *> patternP)
        <*> (sc *> (eq <?> "equality") *> exprP)

-- nonRecDecl = SItem Recursive <$ kwLet <* kwRec <*> (PVar <$> identifierP) <*> exprP

-- varSigP = manyParens $ (,) <$> manyParens identifierP <*> optionalTypeAnnotationP

-- ** Expr Parsers

exprP :: Parser Expr
exprP = makeExprParser exprTerm opsTable

exprTerm :: Parser Expr
exprTerm =
  choice'
    [ parens exprP,
      (\(SItem flg pat body) wher -> ELet flg pat body wher) <$> declP <* kwIn <*> exprP,
      EConst <$> primValExprP,
      kwFun *> funP arrow,
      EIf <$ kwIf <*> exprP <* kwThen <*> exprP <* kwElse <*> exprP,
      EVar <$> identifierP
    ]

-- ** Operation Parsers

opsTable :: [[Operator Parser Expr]]
opsTable =
  [ [appOp],
    -- [unOp "-" UnMinusOp],
    [ binary "*" (EApp . EApp (EVar "*")),
      binary "/" (EApp . EApp (EVar "/"))
    ],
    [ binary "+" (EApp . EApp (EVar "+")),
      binary "-" (EApp . EApp (EVar "-"))
    ]
  ]

binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

appOp :: Operator Parser Expr
appOp = InfixL $ return Parsetree.EApp

-- binLeftOp :: String -> BinaryOperator -> Operator Parser Expr
-- binLeftOp name op = InfixL $ ExprBinOp op <$ symbol name

-- boolOp :: String -> BooleanOperator -> Operator Parser Expr
-- boolOp name op = binLeftOp name $ BoolOp op

-- arithOp :: String -> ArithmeticOperator -> Operator Parser Expr
-- arithOp name op = binLeftOp name $ ArithOp op

-- compOp :: String -> ComparisonOperator -> Operator Parser Expr
-- compOp name op = binLeftOp name $ CompOp op

-- unOp :: String -> UnaryOperator -> Operator Parser Expr
-- unOp name op = Prefix $ ExprUnOp op <$ symbol name

-- ** Type Parsers

-- typeP :: Parser Type
-- typeP =
--   choice'
--     [ TFun <$> primitiveOrInParensTypeP <* arrow <*> typeP,
--       primitiveOrInParensTypeP
--     ]
--   where
--     primitiveOrInParensTypeP = choice' [parens typeP, primitiveTypeP]
--     primitiveTypeP =
--       choice'
--         [ TUnit <$ kwUnit,
--           TBool <$ kwBool,
--           TInt <$ kwInt
--         ]

-- ** Primitive Value Parser

primValExprP :: Parser Const
primValExprP =
  choice'
    [ -- PrimValUnit <$ unitLitP,
      PConst_bool <$> boolLitP,
      PConst_int <$> intLitP
    ]

-- ** Function Parser

funP :: Parser String -> Parser Expr
-- funP sepSymbolP = flip (foldr ELam) <$> some1 parameterP <* sepSymbolP <*> exprP
funP _ = exprP

parameterP :: Parser Pattern
parameterP =
  choice'
    [ manyParens (pvar <$> identifierP)
    ]

-- optionalTypeAnnotationP :: Parser (Maybe Type)
-- optionalTypeAnnotationP = optional' (colon *> typeP)