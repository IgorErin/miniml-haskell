{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Parsetree where

import Data.List

data Const
  = PConst_int Int
  | PConst_bool Bool
  deriving (Show, Eq)

data RecFlag
  = Recursive
  | NonRecursive
  deriving (Show, Eq)

newtype Pattern = PVar String deriving (Show, Eq)

data Expr
  = EConst Const
  | EVar String
  | EIf Expr Expr Expr
  | ELam Pattern Expr
  | EApp Expr Expr
  | ELet RecFlag Pattern Expr Expr
  deriving (Show, Eq)

econst_int n = EConst (PConst_int n)

elams :: [String] -> Expr -> Expr
elams xs e = Data.List.foldr (\x -> ELam (PVar x)) e xs

data StructureItem = SItem RecFlag Pattern Expr deriving (Show, Eq)

newtype Program = Program [StructureItem] deriving (Show, Eq)