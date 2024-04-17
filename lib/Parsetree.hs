module Parsetree where

data Const
  = PConst_int Int
  | PConst_bool Bool
  deriving (Show)

data RecFlag
  = Recursive
  | NonRecursive
  deriving (Show)

newtype Pattern = PVar String deriving (Show)

data Expr
  = EConst Const
  | EVar String
  | EIf Expr Expr Expr
  | ELam Pattern Expr
  | EApp Expr Expr
  | ELet RecFlag Pattern Expr Expr
  deriving (Show)

econst_int n = EConst (PConst_int n)