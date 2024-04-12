module Parsetree where

data RecFlag
    = Recursive
    | NonRecursive

newtype Pattern = PVar String

data Expr
    = EConst Int
    | EVar String
    | EIf Expr Expr Expr
    | ELam Pattern Expr
    | EApp Expr Expr
    | ELet RecFlag Pattern Expr Expr