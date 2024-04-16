module Parsetree where

data RecFlag
    = Recursive
    | NonRecursive
    deriving (Show)

newtype Pattern = PVar String deriving (Show)

data Expr
    = EConst Int
    | EVar String
    | EIf Expr Expr Expr
    | ELam Pattern Expr
    | EApp Expr Expr
    | ELet RecFlag Pattern Expr Expr
    deriving (Show)