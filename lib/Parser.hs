{-# HLINT ignore "Use first" #-}
{-# LANGUAGE LambdaCase #-}
-- https://github.com/AzimMuradov/miniml-compiler-haskell-spbu/blob/master/lib/Parser/Parser.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser (Result, Parser, parseProgram, parseExpr, parsePattern, parseTy, parseIdent) where

import Control.Arrow (ArrowChoice (right))
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Bifunctor (bimap)
import Data.List.NonEmpty (some1)
import Data.String (String)
import GHC.Base (TyCon (TyCon))
import GHC.Conc (TVar (TVar))
import Lexer
import Lexer (kwBool, lexeme)
import ParserUtils
import Parsetree
import Text.Megaparsec (MonadParsec (..), choice, many, option, optional, parse, parseMaybe, (<?>))
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (errorBundlePretty)

type Type = Parsetree.Ty

type Identifier = String

type Result = Either String Expr

runWrap p str =
  bimap errorBundlePretty id $
    (parse $ sc *> p <* eof) "file.ml" str

-- | Parser entry point
parseProgram :: String -> Either String Program
parseProgram = runWrap programP

parseIdent :: String -> Either String Identifier
parseIdent = runWrap Lexer.identifierP

parsePattern :: String -> Either String Pattern
parsePattern str =
  bimap errorBundlePretty id $
    (parse $ sc *> patternP <* eof) "file.ml" str

parseExpr :: String -> Either String Expr
parseExpr str = bimap errorBundlePretty id foo
 where
  foo = (parse $ sc *> exprP <* eof) "file.ml" str

parseTy :: String -> Either String Ty
parseTy str = bimap errorBundlePretty id foo
 where
  foo = (parse $ sc *> typeP <* eof) "file.ml" str

-- * Internal

programP :: Parser Program
programP = Program <$> many declP

-- stmtP :: Parser Statement
-- stmtP = choice' [StmtExpr <$> exprP, StmtDecl <$> declP]

-- ** User Declaration Parsers

typeP :: Parser Ty
typeP = makeExprParser annotatedTy typOps
 where
  typTerm :: Parser Ty
  typTerm =
    choice
      [ TyVar <$> (lexeme "'" *> intLitP)
      , Prm <$> identifierP
      , Prm "unit" <$ kwUnit
      , Prm "bool" <$ kwBool
      , parens typeP
      ]

  annotatedTy :: Parser Ty
  annotatedTy =
    typTerm
      <* optional'
        ( lexeme "@"
            <* choice
              [ keyword "unique"
              , try $ keyword "local" <* sc <* keyword "exclusive"
              , keyword "local"
              ]
        )
  typOps :: [[Operator Parser Ty]]
  typOps =
    [ [InfixL $ return Parsetree.TApp]
    ,
      [ InfixL (Parsetree.Arrow <$ symbol "->")
      ]
    ]

patternP :: Parser Pattern
patternP =
  choice
    [ leftPar
        *> choice
          [ try $ (`PVar` PMUnique) <$> (keyword "unique" *> identifierP <* rightPar)
          , try $ (`PVar` PMLocalExclusive) <$> (keyword "local" *> keyword "exclusive" *> identifierP <* rightPar)
          , try $ PAscr <$> identifierP <* sc <* keyword ":" <*> typeP <* rightPar
          , try $ (`PVar` PMNone) <$> (identifierP <* rightPar)
          , try $ patternP <* rightPar
          , PUnit <$ rightPar
          ]
    , pvar <$> identifierP
    , PAny <$ (sc *> Lexer.keyword "_")
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
    [ parens exprP
    , -- , EBorrowVar <$> (keyword "&" *> identifierP)
      (\(SItem flg pat body) wher -> ELet flg pat body wher) <$> declP <* kwIn <*> exprP
    , EConst <$> primValExprP
    , kwFun *> funP arrow
    , EIf <$ kwIf <*> exprP <* kwThen <*> exprP <* kwElse <*> exprP
    , EVar <$> identifierP
    ]

-- ** Operation Parsers

opsTable :: [[Operator Parser Expr]]
opsTable =
  [ [Prefix (EBorrow <$ symbol "&")]
  , [appOp]
  , -- , [Prefix (EBorrowVar <$> (symbol "&" *> p))]

    [ unOp "-" (EApp (evar "-"))
    ]
  ,
    [ binary "*" (EApp . EApp (EVar "*"))
    , binary "/" (EApp . EApp (EVar "/"))
    ]
  ,
    [ binary "+" (EApp . EApp (EVar "+"))
    , binary "-" (EApp . EApp (EVar "-"))
    ]
  ]

binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

unOp :: String -> (Expr -> Expr) -> Operator Parser Expr
unOp name f = Prefix (f <$ symbol name)

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
      PConst_bool <$> boolLitP
    , PConst_int <$> intLitP
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
